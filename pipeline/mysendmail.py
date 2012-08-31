import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEImage import MIMEImage
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
import os

def sendMail(to, subject, text, files=[],server="smtp.obs-nancay.fr"):
    assert type(to)==list
    assert type(files)==list
    fro = "Clairvaux <clairvaux@obs-nancay.fr>"

    msg = MIMEMultipart()
    msg['From'] = fro
    msg['To'] = COMMASPACE.join(to)
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = subject

    msg.attach( MIMEText(text) )

    for file in files:
        if '.png' in file:
            part = MIMEImage(file,_subtype="png")
            part.set_payload( open(file,"rb").read() )
            Encoders.encode_base64(part)
	    part.add_header('Content-Disposition', 'attachment;filename="%s"' % os.path.basename(file))

	else:
            part = MIMEBase('application', "octet-stream")
            part.set_payload( open(file,"rb").read() )
            Encoders.encode_base64(part)
            part.add_header('Content-Disposition', 'attachment;filename="%s"' % os.path.basename(file))
        msg.attach(part)

    if 'nancay' in server: 
        smtp = smtplib.SMTP(server, 25)
    elif 'berkeley' in server:	
        smtp = smtplib.SMTP_SSL(server, 465)
        smtp.login("gdesvignes", "0244AlinkA")
    #smtp.set_debuglevel(1)
    smtp.sendmail(fro, to, msg.as_string() )
    smtp.close()

if __name__ == '__main__':
    sendMail(["gdesvignes.astro@gmail.com"], "hello","cheers", ["/home/desvignes/README"], server="calmail.berkeley.edu")
