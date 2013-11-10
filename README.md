mpvguihs
========

A minimalist mpv GUI written in I/O heavy Haskell.

Building:
---------

cabal configure
cabal build

Installing:
-----------

cabal install

Using:
------

Launch the app (mpvguihs). Select a movie. Use the on-screen controls. Standard mpv key-bindings work according to your configuration.

Development and bugs:
---------------------

Pull requests, patches and bug reports accepted through GitHub.

FAQ:
----

1. Why?
I wanted a simple project to work on my Haskell skills.

2. Why Haskell?
Learning a different paradigm.

3. Did it work?
Well... a GUI is probably not the best way to learn Haskell. Too much I/O (nope, no FRP here).

4. Oh, god, so many bugs. wTF?
Release early, release often.

5. Development plan?
None. I will work on this when I can. Features will be implemented according to interest. Use the bug tracker to request features and vote for them.

6. This sucks, I cannot even pick the subtitles!
Use mpv's keybindings or the OSD. If you want an UI for that, see point 5.

7. This is not Haskell. This is C-style Haskell code. 
This. I need your advice on this. If you have a suggestion, use the bug-tracker or contact me directly.
