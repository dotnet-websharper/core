/// NOTE: temporary solution for #203:
/// https://bitbucket.org/IntelliFactory/websharper/issue/203/dom-identification-in-websharper
declare module IntelliFactory.WebSharper {

    export module DomUtil {
        interface _Attr extends Attr { }
        interface _Element extends Element { }
        interface _Node extends Node { }
        interface _Text extends Text { }
    }

    export module Dom {
        import U = DomUtil;
        interface Attr extends U._Attr { }
        interface Element extends U._Element { }
        interface Node extends U._Node { }
        interface Text extends U._Text { }
    }
}
