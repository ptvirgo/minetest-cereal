# minetest-cereal

What can I find out about the [Minetest](http://www.minetest.net/) world
database format?

The [How to Code - Systematic Program
Design](https://www.edx.org/xseries/how-code-systematic-program-design)
course has shown me that if you design your data well, it's easy to
write good clean code.  "Good" meaning code that is well structured and
easy to understand.  Even better, well designed data can guide you to be
more effective at actually writing complicated algorithms.

When I got curious about how Minetest represents worlds, I was
pretty excited to discover that they use a SQLite database to store the
map.  I'm comfortable with SQL, and expected to be able to
jump in and start reshaping imaginary planets.

No such luck!  Turns out their data structure was put together
more or less by accident.  It consists of a single mathematically
derived number to represent the X,Y,Z coordinates of a given block, and
a binary blob that serializes whatever information there is to know
about that block.  Neither of these representations is readily
comprehensible by humans.

This project will attempt to decode databases in the existing Minetest
format into one that fits the standards of the Systematic Program Design
course.  I want to make it easier to alter and explore Minetest
world maps as data, and to convince you that data should be designed so
that people can understand it too.
