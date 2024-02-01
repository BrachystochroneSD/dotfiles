import hashlib, requests, locale, re, sys

ENC = locale.getpreferredencoding()

def isPwned(password):
    """Check if the password has been pwned by calling
    the pwnedpasswords api
    Returns: Boolean
    """
    encodepass=password.encode(ENC)
    hashed=hashlib.sha1(encodepass).hexdigest()
    url = 'https://api.pwnedpasswords.com/range/'+hashed[0:5]
    try:
        r = requests.get(url)
    except Exception:
        return None
    if re.search(hashed[5:],r.text,re.IGNORECASE):
        return True
    else:
        return False


print(isPwned(sys.argv[1]))
