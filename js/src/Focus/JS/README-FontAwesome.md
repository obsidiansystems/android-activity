To get started using Font Awesome in your new or existing Reflex-Dom project, use the fontAwesomeCDN function to create a <link> tag that references the MaxCDN Bootstrap within the widgetHead of your project. Here is an example of how to get started:

```Haskell 
import Reflex.Dom

main :: IO ()
main = mainWidgetWithHead theHead theBody

theHead :: DomBuilder t m => m ()
theHead = do 
    fontAwesomeCDN

theBody :: DomBuilder t m => m ()
theBody = do 
    el "h1" $ text "Hello World"
```


We have madde use of Web.FontAwesomeType module of the font-awesome-type package's enumerations in order to create more type safe functions. 

FAConf has also been added to make FontAwesome configurations type safe as well. The FAConf instance "def" can be used to
create simple icons. 

FontAwesome Icon Widgets can now be created as so:
```Haskell
icon :: DomBuilder t m => FontAwesome -> FAConf -> m ()

icon FaReddit def -- creates a Reddit icon 
```

FOR A COMPLETE LIST OF FONT AWESOME TYPES, visit the hackage documentation below:

http://hackage.haskell.org/package/font-awesome-type-0.1/docs/Web-FontAwesomeType.html#t:FontAwesome
