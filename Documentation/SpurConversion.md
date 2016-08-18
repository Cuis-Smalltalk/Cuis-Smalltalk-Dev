Converting Cuis to the Spur image format. Updating Cuis after update #2883.
================================================

Prior to update #2883, Cuis runs on the cog_HostPlatformName_squeak.stack.v3_timeStamp and (jitted, faster) cog_HostPlatformName_squeak.cog.v3_timeStamp VMs from http://opensmalltalk.org/ (repo at https://github.com/OpenSmalltalk VM development led by Eliot Miranda). Immediately after #2883, Cuis can be converted to the new Spur ObjectMemory and image format, in the 32 bit variant. (Conversion Spur32 <-> Spur64 is easy, and can be done in both directions anytime).

This image format is compatible with the cog_HostPlatformName_squeak.stack.spur_timeStamp and (jitted, faster) cog_HostPlatformName_squeak.cog.spur_timeStamp VMs, also from OpenSmalltalk.

The procedure for doing the conversion is described here, and is easy enough for any Smalltalk developer to do it. This ensures that the Cuis update process is reproducible (as it has always been) and that (if you prefer) you can migrate your own images, instead of starting from a fresh one.

One special feature of the Cuis update process and resulting images is that both the Spur and non-Spur variants share 100% of the source code in the image, and updates. This means that there is no forking! Cuis will keep having a single update stream, and both Spur and non-Spur images will be kept in perfect sync. It also means that you can develop on any of them, and your code will run on both, without any changes. It also means that the choice of image format and VM is done later, at deployment, and doesn't affect development.

Now, the update procedure.

- Pull updates from the Cuis GitHub repo
- Start Cuis as usual
- WorldMenu / Changes... / Install New Updates
- Get this message:
"This image is now ready to be converted to Spur format.
The image will now be saved and quitted.
You can start the new image and update again, to have an updated non-Spur image.
Or you can run the Spur bootstrap on it, and then start it with a Spur 32bit VM to have an updated Spur image."
- Accept it
- In addition to your original image, now you have Cuis4.2-2882-SpurReady.image . This image is ready to be converted to Spur by the Spur bootstrap process developed by OpenSmalltalk to migrate to Spur images of the Cuis, NewSpeak, Squeak and Pharo dialects. To make the conversion reproducible, in the SqueakSpurVMMakerImage directory we included the very Squeak VMMaker development image we used to convert Cuis. It was built following http://www.mirandabanda.org/cogblog/build-image/ . If you want to contribute to OpenSmalltalk VM development, you'd use the latest from https://github.com/OpenSmalltalk . Start the VMMaker image (using a spur 32 bit VM):
	cogspurlinuxht/squeak Cuis-Smalltalk-Dev/SqueakSpurVMMakerImage/SpurVMMaker.image
- Open a Workspace. Evaluate:
	SpurBootstrap32 bootstrapCuisImage: '../Cuis4.2-2882-SpurReady.image'.
- Wait. It might take around 1 hour, maybe 2. Result will be Cuis4.2-2882-SpurReady-spur.image. Quit the VMMaker Squeak image.

- start Cuis4.2-2882-SpurReady.image with a non-Spur VM.
	coglinuxht/squeak Cuis-Smalltalk-Dev/Cuis4.2-2882-SpurReady.image
- WorldMenu / Changes... / Install New Updates
- WorldMenu / Save options... / Save as New Version

- start Cuis4.2-2882-SpurReady-spur.image with a Spur VM.
	cogspurlinuxht/squeak Cuis-Smalltalk-Dev/Cuis4.2-2882-SpurReady-spur.image
- WorldMenu / Changes... / Install New Updates
- WorldMenu / Save options... / Save as New Version

Done. Now you have Cuis in both Spur and non-Spur flavors!
