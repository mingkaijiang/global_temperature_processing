import sys
import argparse
import os

from random import SystemRandom
from uuid import uuid4

# Revised version by Matt Pritchard, CEDA/STFC to work with globus-cli

def listEndpoints(gendpointDict):

    endNames = list(gendpointDict.keys())
    print("Endpoints involved:")
    for thisEndName in endNames:
        print(thisEndName)

def arguments(argv):

    parser = argparse.ArgumentParser(description = \
        '''To use this script, you must have the Globus Command Line Interface
        tools installed locally (see https://docs.globus.org/cli/)
        The host where you install these tools does
        NOT need to be one of the endpoints in the transfer.
        This script makes use of the Globus CLI 'transfer' command.
        You need to ensure the endpoints involved are activated, see "Endpoints
        to be activated" in output (use "globus endpoint activate")
        By default, the transfer command will:
        - verify the checksum of the transfer
        - encrypt the transfer
        - and delete any fies at the user endpoint with the same name.'''
            )
    parser.add_argument('-e', '--user-endpoint', type=str, help='endpoint you wish to download files to', required=True)
    parser.add_argument('-u', '--username', type=str, help='your Globus username', required=True)
    parser.add_argument('-p', '--path', type=str, help='the path on your endpoint where you want files to be downloaded to', default='/~/')
    parser.add_argument('-l', '--list-endpoints', help='List the endpoints to be activated and exit (no transfer attempted)', action='store_true')
    parser._optionals.title = 'required and optional arguments'
    args = parser.parse_args()

    username = args.username
    uendpoint = args.user_endpoint
    upath = args.path
    listonly = args.list_endpoints

    if '/' in uendpoint:
        print("Do not include the download path in the endpoint name, please use the -p option")
        sys.exit()
    if '#' in upath:
        print("The '#' character is invalid in your path, please re-enter")
        sys.exit()
    if upath[0] != '/' and upath != '/~/':
        upath = '/' + upath

    return (uendpoint, username, upath, listonly)

def getFiles(gendpointDict, uendpoint, username, upath):

    label = str(uuid4())

    endNames = list(gendpointDict.keys())

    for thisEndName in endNames:

        fileList = gendpointDict[thisEndName]

        cryptogen = SystemRandom()
        transferFile = '/tmp/transferList_' + thisEndName + '_' + str(cryptogen.randint(1,9999)) + '.txt'
        file = open(transferFile, 'w')

        for thisFile in fileList:

            basename = os.path.basename(thisFile)

            if upath[-1] != '/':
                basename = '/' + basename

            remote = thisFile
            local = upath + basename

            file.write(remote + ' ' + local + '\n')

        file.close()

        os.system("globus transfer "+thisEndName+" "+uendpoint+" --batch --label \"CLI Batch\" < "+transferFile)

        os.remove(transferFile)

    return

if __name__ == '__main__':

    gendpointDict = {'2058c7d6-a79f-11e6-9ad6-22000a1e3b52': ['/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2059010106-2060010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2060010106-2061010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2061010106-2062010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2062010106-2063010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2063010106-2064010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2064010106-2065010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2065010106-2066010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2066010106-2067010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2067010106-2068010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2068010106-2069010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2069010106-2070010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2070010106-2071010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2071010106-2072010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2072010106-2073010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2073010106-2074010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2074010106-2075010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2075010106-2076010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2076010106-2077010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2077010106-2078010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2078010106-2079010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2079010106-2080010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2080010106-2081010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2081010106-2082010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2082010106-2083010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2083010106-2084010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2084010106-2085010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2085010106-2086010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2086010106-2087010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2087010106-2088010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2088010106-2089010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2089010106-2090010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2090010106-2091010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2091010106-2092010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2092010106-2093010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2093010106-2094010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2094010106-2095010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2095010106-2096010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2096010106-2097010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2097010106-2098010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2098010106-2099010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2099010106-2100010100.nc', '/master/CMIP5/output1/CSIRO-BOM/ACCESS1-0/rcp85/6hr/atmos/6hrLev/r1i1p1/v20121003/ta/ta_6hrLev_ACCESS1-0_rcp85_r1i1p1_2100010106-2101010100.nc']}
    uendpoint, username, upath, listonly = arguments(sys.argv)
    if (listonly):
        listEndpoints(gendpointDict)
    else:
        getFiles(gendpointDict, uendpoint, username, upath)