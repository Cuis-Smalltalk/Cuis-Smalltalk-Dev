'From Cuis 6.0 [latest update: #5970] on 7 August 2023 at 11:04:39 am'!

!Sample04Pacman methodsFor: 'drawing' stamp: 'mnqpr 8/7/2023 11:02:59'!
drawOn: aCanvas

	aCanvas fillColor: color do: [
		aCanvas circleCenter: 30@60 radius: 15.
		aCanvas
			moveTo: 0@0;
			lineTo: 100@ -50;
			arcTo: 100@ 50 radius: 110 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			lineTo: 0@0.
		].! !

