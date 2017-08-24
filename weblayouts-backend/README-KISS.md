KISS (Keep It Super Simple) Layout

The new Focus weblayout, KISS, allows you to quickly prepare a responsive
single page web application with backend routing in place. 

The cabal packages for this layout are identified as: 
  - focus-weblayouts 
  - focus-weblayouts-backend

The modules that will need to be imported are: 
  - Focus.Weblayouts.Kiss within your 'frontend/' and 'common/' directories
  - Focus.Weblayouts.Backend.KissBackend within your 'backend/' directory

The function that pulls most of the weight in putting your website together
will be the 'bodyGen' function, short for body Generator. 
  Note: Type signature have been shortened for simplicity

```haskell 
bodyGen :: DomBuilder t m => Text -> [a] -> a -> m ()
```

The first 'Text' parameter will be a path to the image or logo you would like
at the top of the page. 

The second '[a]' parameter takes a data type that represents each page you would 
like your website to change to. This is important because what appears in the 
NavBar, what appends to the URI, and what widgets will be loaded depend on this 
argument. 
  - It is best to specify all of this information in the 'common/' directory,
    for example: 

      /common/src/Common.hs
      ```haskell
      data Pages = PageOne | PageTwo | Page3 

      -- Along with functions and widgets to aid with redirects using the
      WebRoute typeclass (found in Focus.Weblayout.Kiss module)

      instance WebRoute Pages where 

        routeToTitle :: Pages -> Text
        routeToTitle r = case r of 
          PageOne -> "Page One" 
          PageTwo -> "Page Two" 
          PageThree -> "Page Three"  

        routeToUrl :: Pages -> Text
        routeToUrl r = case r of 
          PageOne -> "/pageone" 
          PageTwo -> "/pagetwo" 
          PageThree -> "/pagethree"          
        
        urlToRoute :: Text -> Maybe Pages
        urlToRoute path = Map.lookup path pages
          where pages = Map.fromList $ fmap (\r ->(routeToUrl r, r)) [PageOne,PageTwo,PageThree] 

				routeToWidget :: DomBuilder t m => Pages -> m ()
        routeToWidget r = case r of 
          PageOne -> pageOneWidget 
          PageTwo -> pageTwoWidget 
          PageThree -> pageThreeWidget 
			```

	- The widgets that 'routeToWidget' will reference should also be placed within this file. 

The third and final argument, is the initial page the website which references one of the 'Pages'

Here is an example when calling 'bodyGen' in 'frontend/src/Frontend/App.hs':
	```haskell
	siteBody :: (DomBulder t m, ...) => Pages -> m ()
	siteBody initPage = do 
		bodyGen siteLogo sitePages initPage
		where
			siteLogo = "path/to/myLogo.png" 
			sitePages = [PageOne, PageTwo, PageThree]
	```

That covers everything on the frontend, now for the backend.

Found within 'Focus.Weblayouts.Backend.KissBackend' is the function 'rootHandler' that 
helps with the asset handling of the website. Here is an example of using Kiss's 
root handler in the 'backend/src/Main.hs' 

	```haskell
	main :: IO ()
	main = do 
		siteHeadByteString <- fmap snd $ renderStatic siteHead
		withFocus . quickHttp $ rootHandler siteHeadByteString siteBody PageOne
	```

Utilize the style.css template found in 'weblayouts/src/Focus/Weblayouts/CSS', double check 
your module imports and type constraints and you should be able to build.
