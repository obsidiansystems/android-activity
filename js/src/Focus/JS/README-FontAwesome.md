To get started using Font Awesome in your new or existing Reflex-Dom project, use the fontAwesomeCDN function to create a <link> tag that references the MaxCDN Bootstrap within the widgetHead of your project.


Recently, We have madde use of Web.FontAwesomeType module of the font-awesome-type package's enumerations in order to create more type safe functions. 

FAConf has also been added to make FontAwesome configurations type safe as well. The FAConf instance "def" can be used to
create simple icons. (More advanced notation to be released)

FontAwesome Icon Widgets can now be created as so:
```Haskell
faIcon :: DomBuilder t m => FontAwesome -> FAConf -> m ()

faIcon FaReddit def -- creates a Reddit icon 
```

New 1g (half sized), 2x...5x sizes are now available. Simply append the desired size to the end of the function name. 

```Haskell
faIcon1g FaTwitter def
faIcon3x FaReddit def
```

FOR A COMPLETE LIST OF FONT AWESOME TYPES, visit the hackage documentation below:

http://hackage.haskell.org/package/font-awesome-type-0.1/docs/Web-FontAwesomeType.html#t:FontAwesome
