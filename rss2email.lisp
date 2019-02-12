#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rss2email
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    rss2email: get RSS feeds emailed to you.
;;;;
;;;;    Usage:
;;;;       new [youremail] (create new feedfile)
;;;;       email [yournewemail] (update default email)
;;;;       run [--no-send] [num]
;;;;       add feedurl [youremail]
;;;;       list
;;;;       delete n
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-11-26 <PJB> Converted from python by Aaron Swartz.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2006 - 2006
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

;;;;;; Vaguely Customizable Options ;;;;;;

(defvar *DEFAULT-FROM*  "rss2email@localhost"
  "The email address messages are from by default.")

(defvar *HTML-MAIL* :CONVERT
  "
:HTML       Send text/html messages when possible.
:CONVERT    Convert HTML to plain text.
")

(defvar *FORCE-FROM* nil
    "
T           Only use the *DEFAULT-FROM* address.
NIL         Use the email address specified by the feed, when possible.
")

(defvar *TRUST-GUID*  T
  "
T             Receive one email per post.
NIL           Receive an email every time a post changes.
")

(defvar *DATE-HEADER*  :ITEM
  "
:ITEM         Generate Date header based on item's date, when possible.
:SENT         Generate Date header based on time sent.
")

(defvar *DATE-HEADER-ORDER* '(:modified :issued :created)
  "
A list of (member :issued :created :modified :expired)
giving the order of preference in dates
to use for the Date header of the email.
")

(defvar *QP-REQUIRED*  NIL
  "
T             Apply Q-P conversion (required for some MUAs).
NIL           Send message in 8-bits.
http://cr.yp.to/smtp/8bitmime.html
")

(defvar *VERBOSE*  T
  "
T             Name feeds as they're being processed.
NIL           Keep quiet.
")

(defvar *USE-PUBLISHER-EMAIL*  T
  "
T             Use the publisher's email if you can't find the author's.
NIL           Just use the *DEFAULT-FROM* email instead.
")

(defvar *SMTP-SEND*  NIL
  "
T             Use *SMTP-SERVER* to send mail.
NIL           Call /usr/bin/sendmail to send mail.
")

(defvar *SMTP-SERVER*  "localhost:25")

(defvar *BONUS-HEADER* '()
  "
Set this to add a bonus header to all emails (start with '\n').
Example: (setf *BONUS-HEADER* '(\"Approved: joe@bob.org\"
                                \"Errors-To: joe@bob.org\"))
")

(defvar *OVERRIDE-FROM*  '()
  "
Set this a-list to override From addresses.
Keys are feed URLs, values are new titles.
")

(defvar *sendmail-program* "/usr/sbin/sendmail"
  "Path to the sendmail program.")



(defun hostname ()
  (with-open-stream (input (ext:run-program "hostname" :arguments '("-f") :output :stream))
    (or (read-line input nil nil) (short-site-name))))


(defun sendmail (sender recipients message)
  (with-open-stream
      (smtp
       (let* ((colon (position #\: *smtp-server*))
              (host  (if colon (subseq *smtp-server* colon) *smtp-server*))
              (port  (if colon (parse-integer *smtp-server* :start (1+ colon)) 25)))
         (socket:socket-connect port host
                                :ELEMENT-TYPE 'character
                                :EXTERNAL-FORMAT (ext:make-encoding :charset charset:utf-8
                                                                    :line-terminator :dos)
                                :BUFFERED nil :TIMEOUT 30)))
    (flet ((go-on ()
             (<= 200 (read-from-string (or (read-line smtp nil nil) "400")) 399)))
      (format smtp "HELO ~A~%" (hostname))
      (when (go-on)
        (format smtp "MAIL FROM: <~A>~%" sender)
        (when (and (go-on)
                   (some (lambda (recipient)
                           (format smtp "RCPT TO: <~A>~%" recipient)
                           (go-on))
                         recipients))
          (format smtp "DATA~%")
          (when (go-on)
            (format smtp "~A~%.~%" message))))
      (format smtp "QUIT~%")
      (sleep 3))))


(defun send (from to message)
  ;; Note: You can also override the send function.
  (if *smtp-send*
      (sendmail from (list to) message)
      (with-open-stream (sendmail (ext:run-program *sendmail-program*
                                    :arguments  (LIST "-bm" "-B" "8BITMIME"
                                                      "-f" from to)
                                    :input :stream :output nil))
        (princ message sendmail))))


;;;; html2text options ;;;;

(defvar *UNICODE-SNOB*  T
  "
Use Unicode characters instead of their ascii pseudo-replacements.
")


(defvar *LINKS-EACH-PARAGRAPH* nil
  "
Put the links after each paragraph instead of at the end.
")

(defvar *BODY-WIDTH*  nil
"
Wrap long lines at position.
NIL for no wrapping.
")


;;;;;; Load the Options ;;;;;;

;; Read options from config file if present.
import sys
sys.path.append(".")
try:
	from config import *
except:
	pass
	
;;;;;; Import Modules ;;;;;;

import cPickle as pickle, fcntl, md5, time, os, traceback, urllib2, sys, types
import socket; socket_errors = []
for e in ['error', 'gaierror']:
	if hasattr(socket, e): socket_errors.append(getattr(socket, e))
import mimify; from StringIO import StringIO as SIO; mimify.CHARSET = 'utf-8'
if *SMTP-SEND*: import smtplib; smtpserver = smtplib.SMTP(*SMTP-SERVER*)
else: smtpserver = None

import feedparser
feedparser.USER_AGENT = "rss2email/"+__version__+ " +http://www.aaronsw.com/2002/rss2email/"

import html2text as h2t

h2t.*UNICODE-SNOB* = *UNICODE-SNOB*
h2t.*LINKS-EACH-PARAGRAPH* = *LINKS-EACH-PARAGRAPH*
h2t.*BODY-WIDTH* = *BODY-WIDTH*
html2text = h2t.html2text

;;;;;; Utility Functions ;;;;;;

warn = sys.stderr

def isstr(f): return isinstance(f, type('')) or isinstance(f, type(u''))
def ishtml(t): return type(t) is type(())
def contains(a,b): return a.find(b) != -1
def unu(s): ;; I / freakin' hate / that unicode
	if type(s) is types.UnicodeType: return s.encode('utf-8')
	else: return s

def quote822(s):
	"""Quote names in email according to RFC822."""
	return '"' + unu(s).replace("\\", "\\\\").replace('"', '\\"') + '"'

def header7bit(s):
	"""QP_CORRUPT headers."""
	return mimify.mime_encode_header(s + ' ')[:-1]

;;;;;; Parsing Utilities ;;;;;;

def getContent(entry, HTMLOK=0):
	"""Select the best content from an entry, deHTMLizing if necessary.
	If raw HTML is best, an ('HTML', best) tuple is returned. """
	
	;; How this works:
	;;  * We have a bunch of potential contents.
	;;  * We go thru looking for our first choice.
	;;    (HTML or text, depending on HTMLOK)
	;;  * If that doesn't work, we go thru looking for our second choice.
	;;  * If that still doesn't work, we just take the first one.
	;;
	;; Possible future improvement:
	;;  * Instead of just taking the first one
	;;    pick the one in the "best" language.
	;;  * HACK: hardcoded HTMLOK, should take a tuple of media types
	
	conts = entry.get('content', [])
	
	if entry.get('summary_detail', {}):
		conts += [entry.summary_detail]
	
	if conts:
		if HTMLOK:
			for c in conts:
				if contains(c.type, 'html'): return ('HTML', c.value)
	
		for c in conts:
			if c.type == 'text/plain': return c.value
	
		if not HTMLOK: ;; Only need to convert to text if HTML isn't OK
			for c in conts:
				if contains(c.type, 'html'):
					return html2text(c.value)
		
		return conts[0].value	
	
	return ""

def getID(entry):
	"""Get best ID from an entry."""
	if *TRUST-GUID*:
		if 'id' in entry and entry.id: return entry.id

	content = getContent(entry)
	if content: return md5.new(unu(content)).hexdigest()
	if 'link' in entry: return entry.link
	if 'title' in entry: return md5.new(unu(entry.title)).hexdigest()

def getName(r, entry):
	"""Get the best name."""

	feed = r.feed
	if r.url in *OVERRIDE-FROM*.keys():
		return unu(*OVERRIDE-FROM*[r.url])
	
	name = feed.get('title', '')
	
	if 'name' in entry.get('author_detail', []): ;; normally {} but py2.1
		if entry.author_detail.name:
			if name: name += ", "
			name +=  entry.author_detail.name

	elif 'name' in feed.get('author_detail', []):
		if feed.author_detail.name:
			if name: name += ", "
			name += feed.author_detail.name
	
	return name

def getEmail(feed, entry):
	"""Get the best email_address."""

	if *FORCE-FROM*: return *DEFAULT-FROM*
	
	if 'email' in entry.get('author_detail', []):
		return entry.author_detail.email
	
	if 'email' in feed.get('author_detail', []):
		return feed.author_detail.email
		
	;;TODO: contributors
	
	if *USE-PUBLISHER-EMAIL*:
		if 'email' in feed.get('publisher_detail', []):
			return feed.publisher_detail.email
		
		if feed.get("errorreportsto", ''):
			return feed.errorreportsto
			
	return *DEFAULT-FROM*

;;;;;; Simple Database of Feeds ;;;;;;

class Feed:
	def __init__(self, url, to):
		self.url, self.etag, self.modified, self.seen = url, None, None, {}
		self.to = to		

def load(lock=1):
	feedfileObject = open(feedfile, 'r')
	feeds = pickle.load(feedfileObject)
	if lock:
		fcntl.flock(feedfileObject.fileno(), fcntl.LOCK_EX)
		;;HACK: to deal with lock caching
		feedfileObject = open(feedfile, 'r')
		feeds = pickle.load(feedfileObject)
		fcntl.flock(feedfileObject.fileno(), fcntl.LOCK_EX)

	return feeds, feedfileObject

def unlock(feeds, feedfileObject):
	pickle.dump(feeds, open(feedfile+'.tmp', 'w'))
	os.rename(feedfile+'.tmp', feedfile)
	fcntl.flock(feedfileObject.fileno(), fcntl.LOCK_UN)

;;;;;; Program Functions ;;;;;;

def add(*args):
	if len(args) == 2 and contains(args[1], '@') and not contains(args[1], '://'):
		urls, to = [args[0]], args[1]
	else:
		urls, to = args, None
	
	feeds, feedfileObject = load()
	if feeds and not isstr(feeds[0]) and to is None:
		raise 'NoEmail', "Run `email newaddr` or `add url addr`."
	for url in urls: feeds.append(Feed(url, to))
	unlock(feeds, feedfileObject)

def run(num=None):
	feeds, feedfileObject = load()
	try:
		;; We store the default to address as the first item in the feeds list.
		;; Here we take it out and save it for later.
		if feeds and isstr(feeds[0]): default_to = feeds[0]; ifeeds = feeds[1:]
		else: ifeeds = feeds
		
		if num: ifeeds = [feeds[num]]
		
		for f in ifeeds:
			try:
				if *VERBOSE*: print >>warn, "I: Processing", f.url
				r = feedparser.parse(f.url, f.etag, f.modified)
				
				;; Handle various status conditions, as required
				if 'status' in r:
					if r.status == 301: f.url = r['url']
					elif r.status == 410:
						print >>warn, "W: feed gone; deleting", f.url
						feeds.remove(f)
						continue
				
				http_status = r.get('status', 200)
				http_headers = r.get('headers', {
				  'content-type': 'application/rss+xml',
				  'content-length':'1'})
				exc_type = r.get("bozo_exception", Exception()).__class__
				if http_status != 304 and not r.entries and not r.get('version', ''):
					if http_status not in [200, 302]:
						print >>warn, "W: error", http_status, f.url

					elif contains(http_headers.get('content-type', 'rss'), 'html'):
						print >>warn, "W: looks like HTML", f.url

					elif http_headers.get('content-length', '1') == '0':
						print >>warn, "W: empty page", f.url

					elif hasattr(socket, 'timeout') and exc_type == socket.timeout:
						print >>warn, "W: timed out on", f.url
					
					elif exc_type == IOError:
						print >>warn, "W:", r.bozo_exception, f.url
					
					elif hasattr(feedparser, 'zlib') and exc_type == feedparser.zlib.error:
						print >>warn, "W: broken compression", f.url
					
					elif exc_type in socket_errors:
						exc_reason = r.bozo_exception.args[1]
						print >>warn, "W:", exc_reason, f.url

					elif exc_type == urllib2.URLError:
						if r.bozo_exception.reason.__class__ in socket_errors:
							exc_reason = r.bozo_exception.reason.args[1]
						else:
							exc_reason = r.bozo_exception.reason
						print >>warn, "W:", exc_reason, f.url
					
					elif exc_type == KeyboardInterrupt:
						raise r.bozo_exception

					else:
						print >>warn, "=== SEND THE FOLLOWING TO rss2email@aaronsw.com ==="
						print >>warn, "E:", r.get("bozo_exception", "can't process"), f.url
						print >>warn, r
						print >>warn, "rss2email", __version__
						print >>warn, "feedparser", feedparser.__version__
						print >>warn, "html2text", h2t.__version__
						print >>warn, "Python", sys.version
						print >>warn, "=== END HERE ==="
					continue
				
				r.entries.reverse()
				
				for entry in r.entries:
					id = getID(entry)
					
					;; If *TRUST-GUID* isn't set, we get back hashes of the content.
					;; Instead of letting these run wild, we put them in context
					;; by associating them with the actual ID (if it exists).
					
					frameid = entry.get('id', id)
					
					;; If this item's ID is in our database
					;; then it's already been sent
					;; and we don't need to do anything more.
					
					if f.seen.has_key(frameid) and f.seen[frameid] == id: continue
										
					if 'title_detail' in entry and entry.title_detail:
						title = entry.title_detail.value
						if contains(entry.title_detail.type, 'html'):
							title = html2text(title)
					else:
						title = getContent(entry)[:70]

					title = unu(title).replace("\n", " ")
					
					datetime = time.gmtime()

					if *DATE-HEADER*:
						for datetype in *DATE-HEADER-ORDER*:
							kind = datetype+"_parsed"
							if kind in entry: datetime = entry[kind]
						
					content = getContent(entry, HTMLOK=*HTML-MAIL*)
					
					link = unu(entry.get('link', ""))
					
					from_addr = unu(getEmail(r.feed, entry))

					message = (
					"From: " + quote822(header7bit(getName(r, entry))) + " <"+from_addr+">" +
					"\nTo: " + header7bit(unu(f.to or default_to)) + ;; set a default email!
					"\nSubject: " + header7bit(title) +
					"\nDate: " + time.strftime("%a, %d %b %Y %H:%M:%S -0000", datetime) +
					"\nUser-Agent: rss2email" + ;; really should be X-Mailer
					*BONUS-HEADER* +
					"\nContent-Type: ")         ;; but backwards-compatibility
					
					if ishtml(content):
						message += "text/html"
						
						content = ("<html><body>\n\n" +
						           '<h1><a href="'+link+'">'+title+'</a></h1>\n\n' +
						           unu(content[1]).strip() + ;; drop type tag (HACK: bad abstraction)
						           '<p>URL: <a href="'+link+'">'+link+'</a></p>' +
						           "\n\n</body></html>")
					else:
						message += "text/plain"
						content = unu(content).strip() + "\n\nURL: "+link
					
					message += '; charset="utf-8"\n\n' + content + "\n"

					if *QP-REQUIRED*:
						ins, outs = SIO(message), SIO()
						mimify.mimify(ins, outs)
						message = outs.getvalue()
					
					send(from_addr, (f.to or default_to), message)
			
					f.seen[frameid] = id
					
				f.etag, f.modified = r.get('etag', None), r.get('modified', None)
			except KeyboardInterrupt:
				raise
			except:
				print >>warn, "=== SEND THE FOLLOWING TO rss2email@aaronsw.com ==="
				print >>warn, "E: could not parse", f.url
				traceback.print_exc(file=warn)
				print >>warn, "rss2email", __version__
				print >>warn, "feedparser", feedparser.__version__
				print >>warn, "html2text", h2t.__version__
				print >>warn, "Python", sys.version
				print >>warn, "=== END HERE ==="
				continue

	finally:		
		unlock(feeds, feedfileObject)

def list():
	feeds, feedfileObject = load(lock=0)
	
	if feeds and isstr(feeds[0]):
		default_to = feeds[0]; ifeeds = feeds[1:]; i=1
		print "default email:", default_to
	else: ifeeds = feeds; i = 0
	for f in ifeeds:
		print `i`+':', f.url, '('+(f.to or ('default: '+default_to))+')'
		i+= 1

def delete(n):
	feeds, feedfileObject = load()
	feeds = feeds[:n] + feeds[n+1:]
	print >>warn, "W: feed IDs may have changed, list before deleting again"
	unlock(feeds, feedfileObject)
	
def email(addr):
	feeds, feedfileObject = load()
	if feeds and isstr(feeds[0]): feeds[0] = addr
	else: feeds = [addr] + feeds
	unlock(feeds, feedfileObject)

if __name__ == '__main__':
	ie, args = "InputError", sys.argv
	try:
		if len(args) < 3: raise ie, "insufficient args"
		feedfile, action, args = args[1], args[2], args[3:]
		
		if action == "run":
			if args and args[0] == "--no-send":
				def send(x,y,z):
					if *VERBOSE*: print 'Not sending', (
                        [x for x in z.splitlines() if x.startswith("Subject:")][0])

			if args and args[-1].isdigit(): run(int(args[-1]))
			else: run()

		elif action == "email":
			email(args[0])

		elif action == "add": add(*args)

		elif action == "new":
			if len(args) == 1: d = [args[0]]
			else: d = []
			pickle.dump(d, open(feedfile, 'w'))

		elif action == "list": list()

		elif action == "delete": delete(int(args[0]))

		else:
			raise ie, "invalid action"
		
		if smtpserver:
			smtpserver.quit()
		
	except ie, e:
		print "E:", e
		print
		print __doc__
