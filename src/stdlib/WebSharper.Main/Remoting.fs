// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

module WebSharper.Remoting

open WebSharper.JavaScript

module A = WebSharper.Core.Attributes
module R = WebSharper.Core.Remoting

[<A.JavaScript>]
let mutable EndPoint = "?"

[<A.JavaScript>]
let UseHttps() =
    try
        if not (JS.Window.Location.Href.StartsWith "https://") then
            EndPoint <- JS.Window.Location.Href.Replace("http://", "https://")
            true
        else false
    with _ ->
        // This function is intended to be callable from the top-level in a module,
        // which means that it will be (unnecessarily) called on the server too
        // and throw NotImplementedException. Just silence it.
        false

type Data = string
type Headers = obj
type Url = string

type IAjaxProvider =
    abstract member Async : Url -> Headers -> Data -> (Data -> unit) ->
        (exn -> unit) -> unit

    abstract member Sync : Url -> Headers -> Data -> Data

[<A.Direct @"
    var xhr = new XMLHttpRequest();
    var csrf = document.cookie.replace(new RegExp('(?:(?:^|.*;)\\s*csrftoken\\s*\\=\\s*([^;]*).*$)|^.*$'), '$1');
    xhr.open('POST', $url, $async);
    if ($async == true) {
        xhr.withCredentials = true;
    }
    for (var h in $headers) {
        xhr.setRequestHeader(h, $headers[h]);
    }
    if (csrf) {
        xhr.setRequestHeader('x-csrftoken', csrf);
    }
    function k() {
        if (xhr.status == 200) {
            $ok(xhr.responseText)
        } else if ($csrf && xhr.status == 403 && xhr.responseText == 'CSRF') {
            $csrf();
        } else {
            var msg = 'Response status is not 200: ';
            $err(new Error(msg + xhr.status));
        }
    }
    if ('onload' in xhr) {
        xhr.onload = xhr.onerror = xhr.onabort = k;
    } else {
        xhr.onreadystatechange = function () {
            if (xhr.readyState == 4) {
                k();
            }
        };
    }
    xhr.send($data);
">]
let private ajax (async: bool) (url: Url) (headers: Headers) (data: Data)
    (ok: Data -> unit) (err: exn -> unit) (csrf: unit -> unit) = ()

type XhrProvider [<A.JavaScript>] () =
    interface IAjaxProvider with

        [<A.JavaScript>]
        member this.Async url headers data ok err =
            ajax true url headers data ok err
                (fun () -> ajax true url headers data ok err JS.Undefined)

        [<A.JavaScript>]
        member this.Sync url headers data =
            let res = ref Unchecked.defaultof<_>
            ajax false url headers data
                (fun x -> res := x)
                (fun e -> raise e)
                (fun () ->
                    ajax false url headers data
                        (fun x -> res := x)
                        (fun e -> raise e)
                        JS.Undefined)
            !res

[<A.JavaScript>]
let mutable AjaxProvider = XhrProvider() :> IAjaxProvider

[<A.Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) =
    X<unit>

[<A.JavaScript>]
let makeHeaders (m: string) =
    let headers = obj ()
    (?<-) headers "content-type" "application/json"
    (?<-) headers "x-websharper-rpc" m
    headers

[<A.JavaScript>]
let makePayload (data: obj []) =
    Json.Stringify data

type IRemotingProvider =
    abstract member Sync : string -> obj[] -> obj
    abstract member Async : string -> obj[] -> Async<obj>
    abstract member Send : string -> obj[] -> unit

[<A.JavaScript>]
[<Sealed>]
type AjaxRemotingProvider =

    static member Sync m data : obj =
        let data = AjaxProvider.Sync EndPoint (makeHeaders m) (makePayload data)
        Json.Activate (Json.Parse data)

    static member Async m data : Async<obj> =
        let headers = makeHeaders m
        let payload = makePayload data
        async {
            let! token = Async.CancellationToken
            return! Async.FromContinuations (fun (ok, err, cc) ->
                let waiting = ref true
                let reg =
                    token.Register(fun () ->
                        if !waiting then
                            waiting := false
                            cc (new System.OperationCanceledException())
                    )
                let ok (x: Data) = 
                    if !waiting then
                        waiting := false
                        reg.Dispose()
                        ok (Json.Activate (Json.Parse x))
                let err (e: exn) =
                    if !waiting then
                        waiting := false
                        reg.Dispose()
                        err e
                AjaxProvider.Async EndPoint headers payload ok err)
        }

    static member Send m data =
        Async.Start (Async.Ignore (AjaxRemotingProvider.Async m data))
