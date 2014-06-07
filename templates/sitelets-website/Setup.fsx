(*

# Setup.fsx

Adds registry keys for proper F# Web project support.

**Problem**: pure-F# Web projects do not work well in Visual Studio by default,
in particular adding a new item to the project is not possible.

**Solution**: This will hopefully be addressed in future VS versions.

**Workaround**: The issue with new project items can be worked around by adding
appropriate registry keys. Executing this script does just that.

Credits: Daniel Mohl [1], Mark Seemann.

[1]: http://bloggemdano.blogspot.com/2013/11/adding-new-items-to-pure-f-aspnet.html

*)

open Microsoft.Win32

type RegistryKeyName =
    | HKCU of string
    | HKLM of string

type RegistryKey with

    static member Find(name) =
        let (r, s) =
            match name with
            | HKCU s -> (Registry.CurrentUser, s)
            | HKLM s -> (Registry.LocalMachine, s)
        (r, s.Split([| '\\' |]))
        ||> Seq.fold (fun s t -> s.OpenSubKey(t))

let Guid = "{F2A71F9B-5D33-465A-A702-920D77279786}"

let AddRegistryKeys (product, key) =
    try
        RegistryKey.Find(key)
            .OpenSubKey("LanguageTemplates", true)
            .SetValue(Guid, Guid, RegistryValueKind.String)
        printfn "Added to %s" product
    with _ ->
        printfn "Failed to detect: %s" product

let InstallFSharpWebCapability () =
    [
        "VWD Express 11", HKCU "Software\Microsoft\VWDExpress\11.0_Config\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VWD Express 12", HKCU "Software\Microsoft\VWDExpress\12.0_Config\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VS 11", HKCU "Software\Microsoft\VisualStudio\11.0_Config\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VS WinDesktop Express 12", HKCU "Software\Microsoft\VSWinDesktopExpress\12.0_Config\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VS 12", HKCU "Software\Microsoft\VisualStudio\12.0_Config\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VWD Express 11 (x64)", HKLM "SOFTWARE\Wow6432Node\Microsoft\VWDExpress\11.0\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VWD Express 12 (x64)", HKLM "SOFTWARE\Wow6432Node\Microsoft\VWDExpress\12.0\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VS 11 (x64)", HKLM "SOFTWARE\Wow6432Node\Microsoft\VisualStudio\11.0\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VS WinDesktop Express 12 (x64)", HKLM "SOFTWARE\Wow6432Node\Microsoft\VSWinDesktopExpress\12.0\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
        "VS 12 (x64)", HKLM "SOFTWARE\Wow6432Node\Microsoft\VisualStudio\12.0\Projects\{349C5851-65DF-11DA-9384-00065B846F21}"
    ]
    |> List.iter AddRegistryKeys

InstallFSharpWebCapability ()
