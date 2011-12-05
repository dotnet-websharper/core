// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Remoting

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
    JavaScript.ClientSide<unit>

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
    Async.FromContinuations (fun (ok, err, _) ->
        let ok (x: Data) = ok (Json.Activate (Json.Parse x))
        AjaxProvider.Async EndPoint headers payload ok err)

type A = Async

[<A.JavaScript>]
let Send m data : unit =
    A.Start (A.Ignore (Async m data))
