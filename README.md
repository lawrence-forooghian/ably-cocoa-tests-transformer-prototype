# ably-cocoa-tests-transformer prototype

A prototype for a tool for converting ably-cocoa’s QuickSpec tests to normal XCTestCase subclasses. See https://github.com/ably/ably-cocoa/issues/1201.

The code here is probably awful and shouldn’t be used as an example of how to do anything. It does do what we want it to, though.

Usage:

```
swift run ably-cocoa-tests-linter /Users/lawrence/code/ably/ably-cocoa/Spec/
```

Run with the `--help` flag for information on accepted flags.

You can also run with the `--add-logging` flag and then use the `ably-cocoa-tests-linter-log-comparison` package to compare pre and post migration execution order. (Don’t read too closely into the output of this tool, it seems to show the test suite as having run multiple times when I’m not convinced it did.)
