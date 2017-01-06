library('intradata')

master_collate<-function(race,meetingId,events){
  eventId<-events$EventID[events$Race==race]
  rs<-names(intradata::retrieve_event_info(meetingId,race,'event_competitors'))
  runners<-as.data.frame(matrix(NA,length(rs),2))
  colnames(runners)<-c('EventID','CompID')
  runners$CompID<-rs
  runners$EventID<-eventId
  runners$Race<-race
  runners$MeetingID<-meetingId
  print(runners)
  runners$Distance<-mapply(intradata::retrieve_event_info,runners$MeetingID,runners$Race,'distance')
  runners$Matrix<-mapply(intradata::retrieve_attribute,runners$MeetingID,runners$Race,runners$CompID,'mtx')
  runners$Odds<-mapply(retrieve_attribute,runners$MeetingID,runners$Race,runners$CompID,'odds')
  runners$Course<-toupper(mapply(retrieve_meet_info,runners$MeetingID,'venue_name'))
  runners$FP<-mapply(retrieve_attribute,runners$MeetingID,runners$Race,runners$CompID,'finish_position')
  runners$Race_Name<-mapply(retrieve_event_info,runners$MeetingID,runners$Race,'track_type')
  runners$Include<-mapply(retrieve_event_info,runners$MeetingID,runners$Race,'includeRace')
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
main<-function(meetingId){
  events<-intradata::retrieve_events(meetingId)
  events$MeetingID<-meetingId
  events$Status<-mapply(intradata::retrieve_event_info,events$MeetingID,events$Race,'event_status')
  events<-events[events$Status=="FINAL",]
  events$Fields<-fields<-mapply(intradata::retrieve_field,events$MeetingID,events$Race)
  events<-events[order(events$Race),]
  print(events)
  print('<<<')
  races<-events[!duplicated(events[c("MeetingID","Race")]),c("MeetingID","Race")]
  dat<-as.data.frame(matrix(NA,sum(fields),12))
  colnames(dat)<-c('EventID','CompID','Race','MeetingID','Distance','Matrix','Odds','Course','FP','Race_Name','Include','EventID')
  for(i in 1:nrow(races)){
    end<-sum(events$Fields[1:i])
    if(i==1) start<-1
    else start<-sum(events$Fields[1:(i-1)])+1
    a<-master_collate(events$Race[i],meetingId,events)
    print('>>>')
    print(a)
    dat[start:end,]<-a
    flush.console()
  }
  return(dat)
}
