{ name
, packagePrefix ? "systems.obsidian"
, nixpkgs
, frontend
}:

let inherit (nixpkgs) stdenv;
    appName = name;
    packageName = packagePrefix + "." + name;
    packageSrcDir = "src/" + builtins.replaceStrings ["."] ["/"] packageName;
    androidSdk = nixpkgs.androidenv.androidsdk_6_0_extras;
in stdenv.mkDerivation {
  inherit androidSdk frontend;
  name = "android-app";
  src = ./.;
  builder = builtins.toFile "builder.sh" ''
    source "$stdenv/setup"

    # copy the template project, and then put the src in the right place
    cp -r --no-preserve=mode $src/template/. "$out"
    rm -r $out/src/*
    mkdir -p "$out/${packageSrcDir}"
    cp -r --no-preserve=mode "$src/template/src/systems/obsidian/focus/." "$out/${packageSrcDir}"

    # replace package names in package files
    sed -i 's|      package="systems.obsidian.focus"|      package="'"${packageName}"'"|' $out/AndroidManifest.xml
    sed -i 's|<project name="focus" default="help">|<project name="'"${appName}"'" default="help">|' $out/build.xml

    # replace package names in source files
    sed -i 's|package systems.obsidian.focus;|package '"${packageName}"\;'|' "$out/${packageSrcDir}/MainActivity.java" "$out/${packageSrcDir}/OverridenWebViewClient.java"

    # write out local.properties
    sed -i 's|sdk\.dir=FILL-THIS-IN|sdk.dir='"$androidSdk"'/libexec/android-sdk-linux|' $out/local.properties

    # copy in the frontend
    mkdir -p $out/assets
    cp -r --no-preserve=mode $frontend/bin/frontend.jsexe $out/assets
  '';
}
