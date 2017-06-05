Assuming you've already added SSH keys to both your github and gitlab
accounts, please proceed with the instructions that follow. 

In your new project's git repository:

Download the focus-init script from
https://gitlab.com/obsidian.systems/focus/raw/develop/

The focus-init script will add and update the focus submodule as well as
generate the necessary boilerplate files and folders. 

```bash
chmod 755 focus-init
focus-init myProject
```
Create backend/src/Main.hs and put something like the following in it:

-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Default
import Focus.Backend
import Focus.Backend.Snap
import Snap

main :: IO ()
main = withFocus . quickHttpServe $ rootHandler

rootHandler :: Snap ()
rootHandler =
  route [ ("", serveApp "" $ def)
        ]
-------------------------------------------------------------------------------

Create frontend/src/Main.hs and put something like the following in it:

-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main = mainWidget $ text "Hello, new project!"
-------------------------------------------------------------------------------

Build the frontend by running ./focus/build-frontend

Make a symbolic link to the result so that the backend can find it when run from the project root:

ln -s frontend/src/Main.jsexe/ frontend.jsexe

Now you can try running the backend in GHCi by running

./focus/ghci-backend

and then typing

main

at the GHCi prompt.

Point your web browser at localhost:8000 and everything should work.
