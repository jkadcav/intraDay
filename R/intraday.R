library('meeting')
library('events')
library('market')
library('competitor')

master_collate<-function(date,animal,race,meetingId,events){
  eventId<-events$EventID[events$Race==race]
  runners<-events::retrieve_runners_ns(meetingId,race,eventId)
  runners$EventID<-eventId
  runners$Matrix<-mapply(market::retrieve_competitor_matrix_value,runners$Race,runners$MeetingID,runners$CompID)
  runners$Odds<-market::retrieve_prices(race,meetingId)
  runners$Date<-date
  runners$Animal<-animal
  runners$Course<-toupper(mapply(meeting::retrieve_course,runners$Date,runners$Animal,runners$MeetingID))
  runners$Distance<-mapply(events::event_distance,runners$EventID)
  runners$FP<-mapply(competitor::finish_position,runners$EventID,runners$CompID)
  runners$Date<-NULL
  runners$Animal<-NULL
  runners$Race_Name<-mapply(events::event_name,runners$EventID)
  return(runners)
}

#' Intraday Main Function
#'
#' This function allows you to express your love of cats.
#' @param date Date in unix format YYYY-MM-DD
#' @param animal Venue type e.g. THOROUGHBRED, HARNESS, GREYHOUND
#' @param venueName name of venue (uppercase)
#' @keywords intraday
#' @export
#' @examples
#' main('2016-10-26','THOROUGHBRED','HAPPY VALLEY')
main<-function(date,animal, venueName){
  meetingid<-meeting::retrieve_meeting_id(date,animal,venueName)
  events<-events::retrieve_events(meetingid)
  events$MeetingID<-meetingid
  events$Status<-mapply(events::event_status,events$EventID)
  events<-events[events$Status=="FINAL",]
  events$Fields<-fields<-mapply(events::retrieve_field,events$EventID)-mapply(competitor::scratchings,events$EventID)
  events$Race<-mapply(events::retrieve_races,events$MeetingID,events$EventID)
  events<-events[order(events$Race),]
  print('<<<')
  races<-events[!duplicated(events[c("MeetingID","Race")]),c("MeetingID","Race")]
  dat<-as.data.frame(matrix(NA,sum(fields),11))
  colnames(dat)<-c('Course','CompID','Matrix','Race','MeetingID','Odds','Scratched','EventID','Distance','FP','Race_Name')
  for(i in 1:nrow(races)){
    end<-sum(events$Fields[1:i])
    if(i==1) start<-1
    else start<-sum(events$Fields[1:(i-1)])+1
    a<-master_collate(date,animal,events$Race[i],meetingid,events)
    print('>>>')
    print(a)
    dat[start:end,]<-a
    flush.console()
  }
  dat<-dat[is.finite(dat$Race),]
  return(dat)
}
