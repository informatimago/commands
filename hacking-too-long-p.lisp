;;;; -*- mode:lisp; coding:utf-8 -*-

(setf *random-state* (make-random-state t))

(defparameter *data*
#(

"You know you've been hacking too long when you suddenly realize that
\"Timberland - The Boot Company\" does NOT repair the computers which
do not want to boot."

"You know you've been hacking too long when you see an annoying ad on
TV for a product priced at 139.99 and you wonder if you can stop this
TV SPAM, tracking down the perpetrators by doing a 'whois 139.99.0.0\"
and seeing who owns that network block."

"You know you've been hacking too long when you made it  just in time
to the bus (running) 'cos you were relying on the fact that it's
almost always late, except of course his time, and you start thinking
about how far away you should rig a sensor somewhere up the road to
have your scheduler email you \"the bus is here\" just-in-time for you
to leave...."

"You know you've been hacking too long when you can't remember how to
format a video tape..."

"You know you've been hacking too long when it takes you several tries
to set your alarm clock because you expect it to count in hex (or
maybe octal)."

"You know you've been hacking too long when you actually consider
building an alarm clock that uses hex."

"You know you've been hacking too long when you actually build an
alarm clock that uses hex."

"You know you've been hacking too long when on an ethnic studies
survey, a programming language beats out your native natural language
for language spoken most frequently at home."

"You know you've been hacking too long when you consider attaching a
punch reader to one of your home machines just for old times' sake."

"You know you've been hacking too long when it already has a punch
reader attached...as an original part."

"You know you've been hacking too long when you name your computers
after your relatives."

"You know you've been hacking too long when you name your relatives
after your computers."

"You know you've been hacking too long when conversations with your
friends are stack-based, with actual verbal cues of \"push\", \"pop\",
\"interrupt\", and \"interrupt return\" being commonly used."

"You know you've been hacking too long when you remember anything
involving computers before Bill \"the Dark Lord\" Gates was involved."

"You know you've been hacking too long when you design new
architectures and implement them as virtual machines frequently and
just for fun (am I the only one who does this?)."

"You know you've been hacking too long when you talk to your computers
more frequently than you talk to your human companions."

"You know you've been hacking too long when you don't see what is
unusual about that previous entry (you talk to your computers more
frequently than you talk to your human companions)."

"You know you've been hacking too long when you post a YKYBHTLW
message with more than three entries."

"You know you've been hacking too long when you see the subject line:
\"For Sale: Alfa 164 $18000\" in a forsale group, and think \"at least
he should know how to spell ALPHA when asking for such a ridiculous
price\". And then find out that \"Alfa\" is indeed spelled completely
right."

"You know you've been hacking too long when you get more root
passwords than dates."

"You know you've been hacking too long when you're more excited about
getting root passwords than dates."

"You know you've been hacking too long when you read a novel and, at
the bottom of the page, see the number \"29\" and think \"hmmm ---
more than 1/4 of the book already gone. Can't be! Less must have made
a mistake calculating the percentage\"."

"You know you've been hacking too long when you spend half an hour
trying to find a group of files in your current project that total
_exactly_ one million bytes. Or you do it, but then someone points out
that you are 48576 bytes over and you don't know what they are talking
about..."

"You know you've been hacking too long when  \"By the time I get to
Phoenix,\" comes on the radio, and your reaction is, \"Who is still
running PHOENIX?"

"You Know You've Been Hacking Too Long With The Wrong People
When... Today at work I needed to edit a line of code, and rather than
delete or overwrite the ';' at the end and type another one when I
finished, I hit the  insert key instead. I then wondered (from a
purely metaphysical sense) whether the ';' I ended with could be
considered the same one I started with or not.  I  shared my thoughts
with my cow orkers: one just snorted and went back to work, while the
other used the oppertunity to launch into a discussion involving
faster-than-light travel through alternate dimensions. "

"You haven't been hacking very long when you are still excited about
getting root passwords. You may have been hacking a little longer when
you *don't* want to get the root password, because you want to be safe
from being held responsible for the system administrator's mistakes."

"You haven't been hacking very long when you see a cigarette machine
in a Pub which has the ammount of time it has been switched on for,
and you think '17 hours 22 minutes, that's not a bad uptime for a
cigarette machine'."

"You haven't been hacking very long when you see a sign that says
\"Disabled Toilet\", and wonder which bits you have to set to
re-enable it."

"You haven't been hacking very long when you have a friend called
\"Dot Atkins\" and want to spell her name .@kins."

"You know you've been hacking too long when you tell people at work
that you have a 7-year-old Sun at home, and they correctly parse that
as \"workstation\", not \"offspring\"."

"You know you've been hacking too long when looking at telephones,
your wife says \"I like this one\"; and you answer \"Yes, it has the
best form factor\", then have to think to realize why she was staring
at you..."

))

(format t "~72<~A~>~%" (aref *data* (random (length *data*))))


