import csv
import datetime

headers = {'obp' : 
                    ['measure_id','timestamp','work_uri','country_uri','event_uri','value'],
            'ubiquity-api' :
                    ['event_id','timestamp','measure','URI','value','uploader','country']
                   }

transform = {'measure_id' : None,
             'timestamp' : 'occurred_at',
             'work_uri' : 'doi',
             'country_uri' : None,
             'event_uri' : 'subj.pid',
             'value' : None
}

def cred2hicsv(infilepath, outfilepath, outformat='obp', granularity = None):
    """Convert a Crossref Event Data CSV file to HIRMEOS CSV format

    This function takes a file in the default format Alkim has developed
    for Crossref Event Data and converts it to a file format that matches
    the OBP metrics csv format, and is broadly similar to the HIRMEOS
    altmetrics API structure. The main differences are some column headings.
    """

    outdata = []
    
    with open(infilepath, 'r') as f:
        reader = csv.DictReader(f)
        data = [line for line in reader]
    
    for datum in data:
        d = dict([(k, datum.get(transform[k])) for k in headers[outformat]])
        d['measure_id'] = cred2operasname(datum)

        if d['measure_id'] is not None:
            outdata.append(d)
    ##TODO## Worry about whether timestamps need reformating

    with open(outfilepath, 'w') as f:
        writer = csv.DictWriter(f, fieldnames = headers[outformat])
        writer.writeheader()
        writer.writerows(outdata)

def cred2operasname(ced_entry):
    """Generates an OPERAS metric name based on a line CED csv

    Some of these are not currently real names as the OPERAS website hasn't been updated to include some
    altmetric measures.
    """

    if ced_entry.get('source_id') is not '':
        base_name = { 
                                                'wikipedia' : 'wikipedia/references/v0',
                                                'twitter' : 'twitter/discusses/v0',
                                                'reddit' : 'reddit/discusses/v0',
                                                'reddit-links' : 'reddit/discusses/v0',
                                                'newsfeed' : 'newsfeed/discusses/v0',
                                                'plaudit' : 'plaudit/recommends/v0',
                                                'web' : 'web/mentions/v0',
                                                'wordpressdotcom' : 'wordpressdotcom/discusses/v0',
                                                None : None
                    }[ced_entry.get('source_id')]

        return 'https://temp.metrics.operas-eu.org/ced/{}'.format(base_name)
    else:
        return None

if __name__ == '__main__':
    cred2hicsv('data/obp-cred-events.csv', 'data/obp-hirmeos-events.csv')


    


