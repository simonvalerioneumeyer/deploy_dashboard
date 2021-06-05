from tweets_class import HonestPieceOfWork
import time
import schedule

lake_name = 'de-tweets-datalake'

#should be changed according to aws instance:
#stored_data_destination = '/Dokumente/Barcelona_GSE/Term_3/Master_thesis/bucket/'

#NB: If class calling does not work, we will have to remove it and just define functions, similar to what we did 
#for the datawarehousing.

# schedule an update every other day:
#schedule.every(2).days.do(HonestPieceOfWork.download_bucket(lake_name, stored_data_destination))


# download buckets:
hpow = HonestPieceOfWork()
hpow.download_bucket('de-tweets-datalake', 'bucket/')

#while True:
#    schedule.run_pending()
#    time.sleep(1)