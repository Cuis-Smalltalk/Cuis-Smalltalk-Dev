'From Cuis 5.0 of 7 November 2016 [latest update: #3713] on 26 April 2019 at 8:57:21 am'!

!ZeroDivide commentStamp: '<historical>' prior: 0!
ZeroDivide may be signaled when a mathematical division by 0 is attempted.

It might be argued that x / 0.0 is Float infinity or Float negativeInfinity, with the sign of x; and that x / -0.0 is Float infinity or Float negativeInfinity, with the opposite sign of x. But usually infinities are not considered in numeric code. Client code might chose to catch the exception and resume with an appropriate value.!

