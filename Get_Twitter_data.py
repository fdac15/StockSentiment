from twython import Twython

APP_KEY = 'RDCuCPIL9O6l5hObhgpStqakU'
APP_SECRET = 'mCXLR9g9oNVeoQDWSX4B5AnMzQtvUozig9muKFl55RuZaHN22o'


OAUTH_TOKEN = '156980782-uxD8gQva2kM2lCTwGShBqarLovHbrXTs1S9IRr15'
OAUTH_TOKEN_SECRET = 'OGahmQqSuqltN98KOHeeg91gMN3EDrWGqvUVoerM3mGqj' 

twitter = Twython(APP_KEY, APP_SECRET,
                  OAUTH_TOKEN, OAUTH_TOKEN_SECRET)
results = twitter.cursor(twitter.search, q='Google OR google OR #google OR $GOOG OR #Google OR #GOOGLE', lang='en', count='10', until='2014-12-30')
for result in results:
    print result
