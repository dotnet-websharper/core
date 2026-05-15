# Completion checklist

Before finishing a coding task:
- Check `git status --short` and make sure only intended files were changed.
- Run the most focused relevant tests/builds feasible for the change.
- For compiler/proxy changes, prefer focused regression tests in `tests/WebSharper.Tests/Regression.fs` for F# or `tests/WebSharper.CSharp.Tests/Regression.cs` for C#.
- For broad validation, run `./build CI-Release *> build-output.txt` and inspect expected successful sections.
- Mention any tests not run and why.
- Note release-note updates when a user-visible/unreleased change needs one.