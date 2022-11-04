# Features 

This document describes the different features of the application, grouped by pages as explained below. 

## High-level description

The app UI is composed of several distinct "pages" that fulfill a specific purpose:
  - The Library Page displays a repository of available songs that can be played
  - The Player Page displays a song's chord chart and some playback controls
  - The Editor Page displays a grid and some edition controls to create or edit a song. 


## Library Page

The library page includes:
  - a list of available songs. rows are highlighted when the mouse hovers over a song.
  - a search bar to filter the list of songs
  - a button to add a new song (move to the Editor Page)
  - a button to load/save a song list

Features:
  - When a user clicks on a song row, the app moves to the Player Page of the song

## Player Page 

The player page displays a song's chord chart with playback controls.
It includes:
  - informations about the song: title, artist, style, metric
  - controls to change the playback key and the display key
  - controls to play/pause the song
  - a slider to set the BPM
  - three sliders to set the volume of instruments (piano, drums, bass)
  - a button to delete the song 
  - a button to edit the song (switch to the editor page)
  - a button to move to the library page

Other features: 
  - the "current" chord is highlighted in red
  - if the user clicks on a chord, the playback switches to this chord
  - if a song is playing and the user clicks on "return to library", the song keeps playing until a new song is played or the song is explicitly stopped

## Editor Page 

The editor page displays a song's chord grid and controls to edit it. 

It includes:
  - text forms to input the song's title and composer
  - controls to set the song's style and metric
  - a slider to set the tempo
  - a tunable brush to add chords
  - controls to add/split/merge/remove bars 
  - undo/redo controls 
  - controls to save the modifications / cancel everything

Other features: 
  - when the mouse hovers over a chord cell, its borders are highlighted in red

