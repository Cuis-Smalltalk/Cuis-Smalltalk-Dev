'From Cuis 5.0 of 7 November 2016 [latest update: #3192] on 7 October 2017 at 4:29:18 pm'!

!Float commentStamp: 'jmv 10/7/2017 16:27:55' prior: 0!
A note About Floating Point numbers and Floating Point Arithmetic.

The following is not specific to Cuis or Smalltalk at all. This is about the properties of Float numbers in any computer implementation.

If you haven't done so already, read https://en.wikipedia.org/wiki/Floating-point_arithmetic

But if you find the Wikipedia article too detailed, or hard to read, then try http://fabiensanglard.net/floating_point_visually_explained/ (get past "How Floating Point are usually explained" and read "A different way to explain...").

Other great reads are:
	"Why don't my numbers add up?":
		http://floating-point-gui.de/
and
	"What Every Computer Scientist Should Know About Floating-Point Arithmetic":
		http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
and also maybe
	"Comparing floating point numbers"
		https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/

Now that you read them, and we are on the same boat, some further comments (from jmv):

Floats are (conceptually) approximate real numbers. That's why trig and other trascendental functions always answer Floats. That's why it is ok to round the result of operations. That's why Float is considered more general than Fraction in ST-80 and most Smalltalks. So, when we have a Float value, we must not think about it as a Rational but as a Real (actually as some unknown Real that could hopefully be close to the Rational we can actually represent). Keep this in mind when dealing with Floats, and especially avoid comparing them for equality.

(If you want exact arithmetic, can live in the limited world of Rational numbers, and can afford a slight performance penalty, use Fraction instead.)

There are some things that can be done to increase the confidence you can have on Float results. One is to do an error propagation analysis on the code you are running. This is not easy, but it is done for any widely used numerical method. Then, you can know real bounds and/or estimates of the errors made.

Perhaps you have heard about "interval arithmetic". It is a bit better than simple Float, but doesn't really fix the problems.

The ultimate solution is to do Monte Carlo analysis, with random contamination of inputs. After the Monte Carlo run, it is needed to do statistical analysis of possible correlations between the distributions of the random noise added to imputs and the result of the algorithm.

Additional food for thought: http://www.cs.berkeley.edu/~wkahan/Mindless.pdf . According to this, doing Monte Carlo as described above attacks a slightly different problem. This might be yet another reason (besides performance) to try something like the next paragraph. I (jmv) came up with it, and I don't really know if it has been described and or tried before or not. Mhhh. Maybe a defensive publication is in order.

A possibility that could be a practical solution, being much cheaper than Monte Carlo, but better than interval arithmetic, is to represent each value by 2 Floats: an estimation of the real value (i.e. an estimation of the mean value of the distribution of the corresponding Monte Carlo result), and an estimation of the error (i.e. an estimation of the standard deviation of the corresponding Monte Carlo result). Or perhaps even 3 of them. In addition to the estimation of the real value and an estimation of the error, we could add a hard bound on the error. In many cases it will be useless, because the error can not really be bound. But in those cases where it is possible to bound it, applications could really know about the quality of computed values.

=======================================================================

My instances represent IEEE-754 floating-point double-precision numbers. They have about 16 digits of accuracy and their range is between plus and minus 10^307. Some valid examples are:
	
	8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12

Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point.  It is actually possible to specify a radix for Squeak Float constants.  This is great for teaching about numbers, but may be confusing to the average reader:

	3r20.2 --> 6.66666666666667
	8r20.2 --> 16.25

If you don't have access to the definition of IEEE-754, you can figure out what is going on by printing various simple values in Float hex.  It may help you to know that the basic format is...
	sign		1 bit
	exponent	11 bits with bias of 1023 (16r3FF) to produce an exponent
						in the range -1023 .. +1024
				- 16r000:
					significand = 0: Float zero
					significand ~= 0: Denormalized number (exp = -1024, no hidden '1' bit)
				- 16r7FF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	53 bits, but only 52 are stored (20 in the first word, 32 in the second).  This is because a normalized mantissa, by definition, has a 1 to the right of its floating point, and IEEE-754 omits this redundant bit to gain an extra bit of precision instead.  People talk about the mantissa without its leading one as the FRACTION, and with its leading 1 as the SIGNFICAND.

The single-precision format is...
	sign		1 bit
	exponent	8 bits, with bias of 127, to represent -126 to +127
                    - 0x0 and 0xFF reserved for Float zero (mantissa is ignored)
                    - 16r7F reserved for Float underflow/overflow (mantissa is ignored)
	mantissa	24 bits, but only 23 are stored
This format is used in FloatArray (qv), and much can be learned from the conversion routines, Float asIEEE32BitWord, and Float class fromIEEE32Bit:.

You might also check https://en.wikipedia.org/wiki/IEEE_754_revision

Other great reads (covering broader but interesting issues):
https://randomascii.wordpress.com/2013/07/16/floating-point-determinism/
http://www.ima.umn.edu/2010-2011/W1.10-14.11/activities/Leeser-Miriam/Leeser-GPU-IMA-Jan2011.pdf!

