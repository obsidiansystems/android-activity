In your new project's git repository:

```bash
git submodule add -b develop git@gitlab.com:obsidian.systems/focus
git submodule update --init --recursive
mkdir -p common/src frontend/src backend/src static config
```

Create a default.nix with something like the following contents:

-------------------------------------------------------------------------------
{}: (import ./focus {}).mkDerivation {
  name = "theProjectName";
  version = "0.1";
  commonDepends = p: with p; [
    data-default
    file-embed
  ];
  frontendDepends = p: with p; [
    data-default
    file-embed
    focus-http-th
    focus-js
    ghcjs-dom
    reflex
    reflex-dom
    these
  ];
  backendDepends = p: with p; [
    data-default
    resource-pool
    snap
    snap-core
    snap-loader-static
    snap-server
  ];
}
-------------------------------------------------------------------------------

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
