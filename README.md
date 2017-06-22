Assuming you've already added SSH keys to both your github and gitlab
accounts, please proceed with the instructions that follow. 

In your new project's git repository:

Download the ob-init.hs script from
https://gitlab.com/obsidian.systems/focus/raw/develop/

The ob-init.hs script will add and update the focus submodule as well as
generate the necessary boilerplate files and folders. Enter the following in
your console, followed by your project's name when prompted. 

```bash
chmod 755 ob-init.hs
ob-init.hs
```
Build the frontend by running ./focus/build-frontend

Now you can try running the backend in GHCi by running

./focus/ghci-backend

and then typing

main

at the GHCi prompt.

Point your web browser at localhost:8000 and everything should work.

The ob-init.hs script should have generated a backend/src/Main.hs file that 
resembles the following: 

-------------------------------------------------------------------------------
```
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
```
-------------------------------------------------------------------------------

The ob-init.hs file should have also generated frontend/src/Main.hs that 
resembles the following:  

-------------------------------------------------------------------------------
```
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main = mainWidget $ text "Hello, new project!"
```
-------------------------------------------------------------------------------

Feel free to edit the frontend and backend directories as you see fit.

Thank you for using Obelisk. 
