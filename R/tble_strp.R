# Use this as master for string based entries

.tbl_strp=function(tble,strata,all_strata,data,var,var.order,type,nxt_row,rnd){

  data=data[!is.nan(data[[var]]) & !(data[[var]] == "NaN"),]
  if(sum(is.na(data[[var]]))>0){
    data[is.na(data[[var]]),var]="Missing"
  }
  data[data[[var]]=="missing",var]="Missing"

  # Get variable ordering
  if(var %in% names(var.order)){
    st=var.order[[var]]
  } else {
    st=sort(unique(data[[var]]))
    if("Other" %in% st | "other" %in% st){
      which.other=ifelse("Other" %in% st,"Other","other")
      st=c(st[-which(st==which.other)],which.other)
    }
    if("Missing" %in% st){
      st=c(st[-which(st=="Missing")],"Missing")
    }
  }

  for(i in st){
    tble[nxt_row,2]=i
    for(j in all_strata){
      if(j=="Overall"){
        data_sub=data[,var]
      } else {
        data_sub=data[data[strata]==j,var]
      }

      ####################################
      # section to change for other outputs
      num=sum(data_sub==i,na.rm=TRUE)
      den=length(data_sub)

      tble[nxt_row,j]=paste0(num,"/",den," (",roundWZero(num*100/den,rnd),"%)")
      #####################################
    }
    nxt_row=nxt_row+1
  }



  return(list(tble=tble,nxt_row=nxt_row))

}
