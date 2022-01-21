Reactive Core Circe JSON Validator
==================================

This is a Scala implementation of a [JSON Schema](https://json-schema.org/) validator based upon the 
great [io.circe](https://circe.github.io/circe/) Library.

Note: the validator is in early state and API may not be stable.

## Features

- Implementing JSON Schema 2020-12
- All tests of the [JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite) passing (except `unknownKeyword.json`, `vocabulary.json`)
  
  **Update:** Some tests in newer versions of the Test Suite are failing.

- You can bring your own Downloader (using Tagless Final Pattern)

## Known Limitations

- `dynamicRef` and `dynamicAnchor` are very rough implemented, do not trust them.
- Unknown Keywords are ignored and won't be serialized.
- No support for `$vocabulary`
- JSON Pointers to elements outside of `$defs` are not fully implemented, except when required by the test suite.
- Only JSON Schema 2020-12 and one Circe-Version supported so far.
- The API won't be stable.

## Security Considerations

Regex Patterns are directly given to `java.util.regex.Pattern.compile`. This could be not secure and could lead to [ReDoS](https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS) Attacks.

**Only use this library for trusted JSON-Schemas**

This is BETA-Software. Use at your own risk.

## How to use it

Please have a look in the `examples` folder for some Examples.

There are some main Types

- `Json` the JSON Representation of the Circe-Library.
- `Schema` represents a single JSON-Schema and can be parsed from `Json`.
- `Validator` a single validator validating some JSON
- `ValidationResult` the result of a validation, contains possible violations.
- `DocumentValidator` a validator for a full set of JSON-Schema-Documents. This is what you want to use for Validating
- `SingleDocumentValidator` representing a single document within `DocumentValidator`
- `Downloader` during resolving, it is possible, that additional URLs need to be downloaded.
  The downloader is responsible for that and you can bring your own. There exists a dummy implementation
  which doesn't download anything `Downloader.emptySimple` and an implementation which is using Java-URL-Facilities `Downloader.JavaUrlDownloader`

## License

The library is released under terms of Apache License 2.0

`3rdparty` contans 3rd-Party references

- `JSON-Schema-Test-Suite`, licensed under terms of MIT-License
- `schema` contains the JSON Schema of JSON Schema 2020-12, released unter terms of BSD-License.
