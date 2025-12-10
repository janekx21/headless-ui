# Headless UI

An experiment on using elm for a UI framework that you can extend with user made plugins.

The Framework itself has no featues. Each plugin is a feature and they can depend on each other.

The Basic.elm example inclues the following plugins/features:

- superTextRenderer
  - Renders empty text as "(empty)"
  - Renders text with a length between 50 and 100 as "..."
- headingPlugin
  - Renders headdings with cascading padding and font size
- tabPlugin
  - Renders the with "Tabs" tagged row element as tabs
- basicButtons
  - Renders button styles
  - Style depends on tags like "Active", "Inactive", "Submit"
- basicLineInput
  - Renders line input styles
- superRounder
  - Adds rounding to elements
- tabbedExtra
  - Changed the rendering of tabs to vertical
  - Depends on the tabPlugin
- i18n
  - Translates text
- debugSpace
  - Renders empty space as colored boxes
- basicLineInput2
  - Replaces the style of basicLineInput with a simple border
  - Depends on basicLineInput

Plugins/Features are added and removed while calling the "toHtml" function by chaning the Config struct. The Basic.elm example also uses buttons to add and remove plugins. Look for the "<" button in the top left.


## Getting Started

To run the Basic.elm example:

- Clone the repositoy
- Install elm
- cd into headless-ui/examples
- Run `elm reactor`
- Open your browser at http://localhost:8000/Basic.elm
