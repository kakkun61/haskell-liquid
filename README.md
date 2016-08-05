# Liquid

Liquid template language tools - heavily influenced by the unpublished [liquid haskell lib](https://github.com/pbrisbin/liquid).

[Shopify guide](https://help.shopify.com/themes/liquid/basics) - note - there are some language features missing and some extra type-sanity in this lib.

### Building & Testing

```
> stack setup
> stack build liquid
> stack test liquid:test
```

### Trying it out

So far supported:

If, elsif, else logic

Filters (functions incl. toTitle, toUpper, toLower and replace (with "find", "replace" args.)

Case statements

Json lookup and interpolation

Variable finding in a compiled template

```
> stack ghci liquid
> import Control.Lens
> import Data.Attoparsec.Text
> import Text.Liquid.Parser
> import Data.Aeson
> import Data.Aeson.Lens

> let parsedTemplate :: Maybe [Expr] =  "$templateGoesHere" ^? templateP

or perhaps try interpretation with a json object:

> import Text.Liquid.Renderer
> let testTemplate = "some text prior {{ a.nested[2] | replace:\'foo\',\'bar\' | toUpper }}somesquishedtext{% if user == \'barry\' %} then this text {% else %} yo yo {% endif %}the end!"
> let jsonContext = maybe (object []) id (preview _Value ("{\"a\":{\"nested\":[\"a\",\"b\",\"foobar\"]}, \"user\":\"barry\"}" :: Text))
> interpretWithJson jsonContext testTemplate

```

### Benchmarking

Initial experiments show parsing templates requiring on the order of ~10-100 μs.

Rendering in the example shown in interpret above required on the order of 10 ns.

```
> stack bench
```

### Available Filters

Name | Takes Args? | Arg Types
-----|-------------|----------
toUpper | No | n/a
toLower | No | n/a
toTitle | No | n/a
replace | Yes | Strings x 2 (Find and Replace) e.g. replace: 'foo','bar'
first | No | n/a
last | No | n/a
firstOrDefault | Yes | 1 x string or number
lastOrDefault | Yes | 1 x string or number
renderWithSeparator | Yes | 1 x string
toSentenceWithSeparator | Yes | 2 x string (separator and final separator)
countElements | No | n/a
