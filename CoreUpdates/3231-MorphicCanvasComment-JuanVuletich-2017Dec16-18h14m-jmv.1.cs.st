'From Cuis 5.0 of 7 November 2016 [latest update: #3230] on 16 December 2017 at 6:16:14 pm'!

!MorphicCanvas commentStamp: 'jmv 12/16/2017 18:15:31' prior: 0!
A MorphicCanvas offers 2D drawing services. It works on a 'form', usually the Display. These services are used, for example, in #drawOn: methods.

Subclasses are specific implementations. BitBltCanvas is based on BitBlt, the raster operation invented by Dan Ingalls for Smalltalk, and included in Smalltalk-80 and Squeak. VectorCanvas is based on its VectorEngine, using a novel technique for the rasterization (sampling) of vector graphics, invented by Juan Vuletich.!

