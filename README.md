Chumpy-windows is an emacs library that provides useful window-related functionality that's missing from stock emacs. Chumpy-windows is composed of three libraries: window-jump, spaces, and window-layout. The libraries are grouped together because they're highly complimentary, and also for ease of maintenance.

- [Window-jump](#window-jump)
- [Spaces](#spaces)
- [Window-layout](#window-layout)

Window-jump
===========

Out of the box, emacs provides the other-window function (C-x o) for navigating around your windows. It moves you forward/backward one or more windows depending on the argument you give it. Unfortunately the ordering of the windows isn't very clearly defined, and navigating through the list of windows linearly is pretty clumsy if you have a lot of windows.

Window-jump.el provides a superior window navigation mechanism by letting you specify the direction of the window you want to move to. Call window-jump-right and it takes you to the window to the right of the current window, and similarly for the other directions. This makes navigating complex window layouts very easy.

The core functions are window-jump-left, window-jump-right, window-jump-up, and window-jump-down. You'll want to bind these to some keys.

There are two parameters you might want to customize. wj-wrap controls whether or not to wrap the window jumping behavior. It defaults to nil, but if you set it to t, jumping to the right window when you're already in the rightmost window will take you to the leftmost window instead. wj-jump-frames, when set to t, will jump to windows in other frames as well as the current frame. It defaults to nil.

#### Setup and Requirements

Put window-jump.el somewhere on your load-path, then (require 'window-jump).

It should work with just about any version of emacs, including emacs run in a terminal.

### How it works

Window-jump.el uses ray-box intersection tests to find the closest window in a particular direction. Using a very general vector math approach like this allows window-jump.el to make intelligent decisions about which window to jump to even when you have a very complex window layout. Window-jump.el handles any arbitrary window layout with ease.

### Related libs

Windmove comes with emacs and serves the same purpose as window-jump.el. It's implementation isn't based on ray-box intersection testing, and isn't quite as robust when it comes to handling the more arbitrary layouts you can get when you're using multiple frames. I need to draw up a diagram to explain this.

The emacs wiki has a [discussion](http://www.emacswiki.org/emacs/CategoryWindows) of different ways to navigate your windows.

Spaces
======

Spaces.el lets you easily assign a name to a window configuration, then switch between window configurations by selecting the named space you want. When you combine spaces.el with the normal emacs window splitting commands, managing your emacs window configurations starts to feel similar to using a manual tiling window manager like ratpoison or stumpwm.

The only function you really need is sp-switch-space. You can use it both to switch to an existing space and to create new spaces. A typical session would look something like this.

* Set up some complicated window configuration.
* M-x sp-switch-space. Type "default" and press enter. This creates a new space.
* M-x sp-switch-space. Type "gdb" and press enter. Now you have two spaces, one named "default" and one named "gdb".
* M-x gdb. Run gdb with gdb-many-windows set to t, which launches gdb with a specific window configuration, obliterating your carefully set up window config.
* M-x sp-switch-space. Select the "default" space to reload your previous window config. Use sp-switch-space to switch back and forth between the spaces.

You should obviously bind sp-switch-space to a key for convenience.

Spaces.el has nothing to do with gdb of course. Gdb is just meant as an example. Spaces.el is particularly useful to help manage emacs libs (e.g. ediff) that need to muck around with the window config.

### Setup and Requirements

Put spaces.el somewhere on your load-path, then (require 'spaces).

I've only tested with my standard emacs setup, which is a source checkout of the emacs 24 code. It should work with any emacs that supports ido-mode and has the current-window-configuration function.

### How it works

Internally, spaces.el is just a wrapper around current-window-configuration and set-window-configuration. When you switch spaces, the current window config is stored, and the window config of the newly selected space is loaded.

Selecting a space is done using ido-completing-read, for convenience.

One really annoying thing about current-window-configuration is that it saves the location of the cursor ("point" in emacs terminology) of each window, and resets point to the saved value when you load the window config, even if you've played around with the buffer and moved to some other location since you created the window config. It'd be preferable if set-window-configuration kept point the same as it was when you last visited the buffer. I've seen other people [complaining](http://stackoverflow.com/questions/5830494/windows-configuration-to-registers/) about this.

Before loading a window config, spaces.el stores each buffer's point, and then restores point in each visible buffer after the window config is loaded. This takes care of the "don't save the point" problem.

Window-layout
=============

TODO
