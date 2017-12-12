# Interactive Shape Server

An Interactive SVG Shape Server written in Haskell using the [Scotty](https://hackage.haskell.org/package/scotty) & [Blaze-SVG](https://hackage.haskell.org/package/blaze-svg) Web eDSL languages.  

<img src="/assets/screenshot.png" />

## Requirements
This project requires the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) to run.  

To download the Haskell Tool Stack on Unix, you can run:
```
curl -sSL https://get.haskellstack.org/ | sh
```

## Installation

To install The Interactive Shape Server:

* Clone this repository:
```
git clone https://nating/interactive-shape-server.git
```

* Navigate into the repository's folder & setup stack:
```
cd interactive-shape-server && stack setup
```

* Build the project:
```
stack build
```

## Usage

To run the server, use the command:
```
stack exec interactive-shape-server
```

You can now interact with SVG shapes from your browser at `localhost:3000`.

## SVG Shape Description

An index page with a text-area input is served at localhost:3000 by the Interactive Shape Server where you can describe shapes and their styles and transformations.

Accepted input for describing the SVG shapes is to be of the form:
```
'['
'(' Shape ',' Transform ',' Style ')'
 { ',' '(' Shape ',' Transform ',' Style ')' }
']'
```
*Shapes, Transforms and Styles are defined below.*

### Shapes

Supported shapes:
* `Empty`
* `Rect`
* `Circle`

### Transforms

Supported Transforms:
* `Identity`
* `Translate <Double> <Double>`
* `Scale <Double> <Double>`
* `Rotate <Double>`
* `SkewX <Double>`
* `SkewY <Double>`
* `Compose (<Transform>) (<Transform>)`

### Styles

Supported Styles:
* `Fill <Color>`
* `FillOpacity <Double>`
* `StrokeWidth <Double>`
* `StrokeColor <Color>`
* `StyleCompose (<Style>) (<Style>)`

### Colors

Supported Colors:
* `Black`
* `White`
* `Blue`
* `Green`
* `Red`
* `Silver`
* `Gray`
* `Yellow`
* `Maroon`
* `Olive`
* `Lime`
* `Aqua`
* `Teal`
* `Navy`
* `Fuchsia`
* `Purple`
