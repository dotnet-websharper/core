// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module IntelliFactory.WebSharper.Remoting

open IntelliFactory.WebSharper.JavaScript

module A = IntelliFactory.WebSharper.Core.Attributes
module R = IntelliFactory.WebSharper.Core.Remoting

[<A.JavaScript>]
let mutable EndPoint = "?"

type Data = string
type Headers = obj
type Url = string

type IAjaxProvider =
    abstract member Async : Url -> Headers -> Data -> (Data -> unit) ->
        (exn -> unit) -> unit

    abstract member Sync : Url -> Headers -> Data -> Data

[<A.Direct @"
    var xhr = new XMLHttpRequest();
    xhr.open('POST', $url, $async);
    for (var h in $headers) {
        xhr.setRequestHeader(h, $headers[h]);
    }
    function k() {
        if (xhr.status == 200) {
            $ok(xhr.responseText)
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
    (ok: Data -> unit) (err: exn -> unit) = ()

type XhrProvider [<A.JavaScript>] () =
    interface IAjaxProvider with

        [<A.JavaScript>]
        member this.Async url headers data ok err =
            ajax true url headers data ok err

        [<A.JavaScript>]
        member this.Sync url headers data =
            let res = ref Unchecked.defaultof<_>
            ajax false url headers data
                (fun x -> res := x)
                (fun e -> raise e)
            !res

[<A.JavaScript>]
let mutable AjaxProvider = XhrProvider() :> IAjaxProvider

[<A.Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) =
    JS.ClientSide<unit>

[<A.JavaScript>]
let makeHeaders (m: string) =
    let headers = obj ()
    (?<-) headers "content-type" "application/json"
    (?<-) headers "x-websharper-rpc" m
    headers

[<A.JavaScript>]
let makePayload (data: obj []) =
    Json.Stringify data

[<A.JavaScript>]
let Call m data : obj =
    let data = AjaxProvider.Sync EndPoint (makeHeaders m) (makePayload data)
    Json.Activate (Json.Parse data)

[<A.JavaScript>]
let Async m data : Async<obj> =
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

type A = Async

[<A.JavaScript>]
let Send m data : unit =
    A.Start (A.Ignore (Async m data))
