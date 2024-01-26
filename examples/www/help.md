# Creating your component

Add any valid html content to the editor. This includes script and style thats that you would like to be part of the widget.

# External dependencies

You can use external dependencies just like in traditional html, using style and link tags.
For dependencies such as fonts, you will need to wrap these in a `<head>` tag. Fonts need to be available globally instead of locally. (the same is true for other dependencies you would liek to make available globally in the page)

# Binding elements

Bindings with shinyBound are added directly as a html named attribute in a tag.

There are 2 types of bindings you can add, `ts`(to shiny) and `fs`(from shiny) binds:

- `ts` binds will send a specific html property to shiny.
- `fs` binds allow a specific html property to be updated from shiny

Each type of bind is build in the following way:

`[direction][html property type] = "[html property name]:[shiny state attribute name]"`

For example, lets way we would like to update the background of a given html button tag from a named attribute in shiny called color:

`<button fsStyle="background:color">A button</button>`

From shiny we can now use color as a named attribute both for creating and for updating the component:

```
# ui
component("myID", "<button fsStyle="background:color">A button</button>", color = "red")

# server
updateComponent("myID", color = "red")
```

It is possible to update multiple html properties from the same shiny state attribute.

There are 5 types of html properties that can be user to create your binds:

- `Property`: A html property that can be accessed from the tag element in javascript as `element.property`
- `Attribute`: A html attribute that can be accesed from the tag element in javascript as `element.getAttribute(property)`
- `Style`: A CSS property that can be accesed from the tag element in javascript as `element.style.property`
- `Class`: A class name that is usually named in the tag element or part of `element.classList` in javascript
- `Event`: The name of a valid on event associated with the tag element

Event is specially important as it controls when data is send back to shiny using `ts` binds. **Any component you create should have at least one `tsEvent` bind somewhere in its html definition**

When adding multiple binds to the same `[direction][html property type]` named attribute in a html tag you can separate them using `|`. For example if we would like both the `background` and `font-size` to be editable from shiny in our button:

`<button fsStyle="background:color|font-size:size">A button</button>`
