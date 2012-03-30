module internal IntelliFactory.WebSharper.Sitelets.Offline.Extra

/// copies files specified in extra.files to the html directory of a mobile application
/// takes in the directory in which extra.files is contained and the destination
val CopyFiles : string -> string -> unit