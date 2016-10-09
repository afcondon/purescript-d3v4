# About the examples directory

It would obviously be very nice to put something similar to the bl.ocks.org
system in place that can take gists from Github and host them in a web
environment that encourages discovery and forking of visualization code.

For the present tho, i've just opted to bundle some simple examples here in the repo.

You'll see a directory called `block` - that's the template that i use when
creating an example, you might find it useful too.

The structure of a demo directory is:

* **bower.json** - for any special additional bower libs that the demo needs (ie Purescript packages usually)
* **package.json** - enables you to `npm run build` each example producing an `example.js` in the `dist` directory
* **src**   - purescript code file(s) needed for this demo, i generally get by with just a `module Main`
* **dist** - contains an index.html which includes the compliled source. you might need to add includes and even
  possibly add additional `npm` modules
