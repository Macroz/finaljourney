Final Journey
=============

Final Journey is a minimalist game for [Ludum Dare #26 Game Programming Convention](http://www.ludumdare.com/compo/) made by [Markku Rontu / markku.rontu@iki.fi / @zorcam](http://markku.rontu.net). Can you survive?

This HTML5 web game is made in [ClojureScript](https://github.com/clojure/clojurescript).

The main libraries used are:
- [Kinetic.js](http://kineticjs.com/) for drawing, 
- [BoxBox](http://incompl.github.io/boxbox/) for physics simulation and 
- [Hammer.js](http://eightmedia.github.io/hammer.js/) for touch support.

Audio is generated with [as3sxfr](http://www.superflashbros.net/as3sfxr/) and played through HTML5 Audio.

Google Chrome is recommended because it is fast enough. Safari is dog slow, Firefox better.

Unfortunately iPad seems to hang on collisions, otherwise it plays well. Let's fix it sometime!

BoxBox is a wrapper on Javascript port of Box2D. I had to patch BoxBox with a few new features and fixes, so I will send pull requests sometime after the competition.

The game is running on [Heroku](https://www.heroku.com/).

[Begin Journey](http://finaljourney.herokuapp.com/)
---------------------------------------------------
