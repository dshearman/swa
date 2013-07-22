import urllib2
from BeautifulSoup import BeautifulSoup

bom_url = "http://www.bom.gov.au/nsw/forecasts/sydney.shtml"

# download the HTML
html = urllib2.urlopen(bom_url).read()

# parse HTML
soup = BeautifulSoup(html)

# find the temperature in the HTML
max_temperature = soup.html.body.findAll('em',{"class" : "max"})[0].string

# print the result
print "The maximum temperature for today is " + max_temperature + " degrees."
