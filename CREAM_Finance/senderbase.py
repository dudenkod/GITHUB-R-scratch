#!/usr/bin/python

# Disclaimer: This script is only for educational purposes.
# Please use this at your own risk.
# Author: jsinix(jsinix.1337@gmail.com) 

import urllib2, argparse
import json, sys

helpMsg = """
SenderBase scores are assigned to IP addresses based on a combination of factors, including email volume and reputation.
Reputation scores in SenderBase may range from -10 to +10, reflecting the likelihood that a sending IP address is trying 
to send spam. Highly negative scores indicate senders who are very likely to be sending spam; highly positive scores 
indicate senders who are unlikely to be sending spam.
SenderBase is a designed to help email administrators better manage incoming email streams by providing objective data 
about the identity of senders. SenderBase is akin to a credit reporting service for email, providing data that ISPs and 
companies can use to differentiate legitimate senders from spam sources. SenderBase provides objective data that allows 
email administrators to reliably identify and block IP addresses originating unsolicited commercial email (UCE) or to 
verify the authenticity of legitimate incoming email from business partners, customers or any other important source. 
What makes SenderBase unique is that it provides a global view of email message volume and organizes the data in a way 
that it is easy to identify and group related sources of email. SenderBase combines multiple sources of information to 
determine a "reputation score" for any IP address. This information includes:
- Email volume information provided by tens of thousands of organizations that regularly receive Internet email
- Spam complaints received by the SpamCop service
- Information on other DNS-based blacklists
Reference: http://www.cisco.com/c/en/us/support/docs/security/email-security-appliance/118380-technote-esa-00.html
"""

def getmx(domin):
    try:
        ipArr = []
        answers = dns.resolver.query(domin, 'MX')
        for rdata in answers:
            ipArr.append(str(rdata.exchange))
        return ipArr
    except Exception as e:
        return None

def getarec(hostfqdn):
    hostfqdnResolved = ''
    try:
        hostfqdnResolved = socket.gethostbyname(str(hostfqdn))
    except Exception as e:
        print e
        hostfqdnResolved = None
    return hostfqdnResolved

def returndomain(simIP):
    ipaddrArrRev = '.'.join(reversed(simIP.split('.')))
    return ipaddrArrRev  + ".rf-adfe2ko9.senderbase.org"

def gettxt(domin):
    try:
        answers = dns.resolver.query(domin, 'TXT')
        for rdata in answers:
            return rdata
    except Exception as err:
        return None

def process_arguments(args):
    parser = argparse.ArgumentParser(description="Senderbase score query")
    parser.add_argument('-i',
                        '--ip',
                        help="IP address to query"
                        )
    parser.add_argument('-d',
                        '--domain',
                        help="Domain name to query"
                        )
    parser.add_argument('-w',
                        '--wiki',
                        action='store_true',
                        help="Wiki for this tool"
                        )
    options = parser.parse_args(args)
    return vars(options)

if len(sys.argv) < 2:
    process_arguments(['-h'])
userOptions = process_arguments(sys.argv[1:])

if userOptions["ip"] != None:
    print "IP: %s\tScore: %s" %(userOptions["ip"], str(gettxt(returndomain(userOptions["ip"]))).replace('"', ''))

if userOptions["domain"] != None:
    userDomain = userOptions["domain"]
    allmx = getmx(userDomain)
    if allmx != None:
        for eachmx in allmx:
            tempdom = returndomain(getarec(eachmx))
            print "MX: %s\tIP: %s\tScore: %s" %(eachmx[:-1], getarec(eachmx), str(gettxt(returndomain(getarec(eachmx)))).replace('"', ''))

if userOptions["wiki"] != None:
    print helpMsg
    sys.exit()
