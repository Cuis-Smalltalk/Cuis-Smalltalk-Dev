Converting Cuis from 32 bits Spur format to 64 bits Spur format (and back)
==============================================

The Spur image format by Eliot Miranda and www.opensmalltalk.org supports both 32 and 64 bits variants of the Smalltalk image. Conversion is simple and can be done anytime. The conversion requires a SpurVMMaker Squeak image. We use the same we used for the original conversion of Cuis from the old "V3" format to Spur 32 bits. It is available in the SqueakSpurVMMakerImage directory.It was built following http://www.mirandabanda.org/cogblog/build-image/ . If you want to contribute to OpenSmalltalk VM development, you'd use the latest from https://github.com/OpenSmalltalk . Start the VMMaker image (using a spur 32 bit VM):
```
cogspurlinuxht/squeak Cuis-Smalltalk-Dev/SqueakSpurVMMakerImage/SpurVMMaker.image
```

- Open a Workspace. Evaluate:
```
Spur32to64BitBootstrap new bootstrapImage: '../Cuis5.0-xxxx-spur.image'
```

- You will get an assertion failure message. Just [continue] in the debugger. Result will be *Cuis5.0-xxxx-spur-64.image*. Quit the VMMaker Squeak image.

Done. Now you have Cuis in Spur 64 bits!
