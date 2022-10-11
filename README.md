# Knivesjam

Knivesjam is a free web application that generate musical backing-tracks from chord sheets.

## Usage 

The app is currently hosted at the following URL: `https://knivesjam.netlify.app`. 

One can also compile and run the app manually. The app is written in Elm (v0.19.1), please find instructions about Elm installation (here)[https://guide.elm-lang.org/install/elm.html]. 

The app can be compiled and run as follow: 
``` 
elm make src/Main.elm --output=elm.js
python -m "http.server"
# Open browser at URL: http://localhost:8000
```
