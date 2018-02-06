DT_RU_LOCALE=function(){
  return(list(
    processing= "���������...",
    search= "�����:",
    lengthMenu= "�������� _MENU_ �������",
    info= "������ � _START_ �� _END_ �� _TOTAL_ �������",
    infoEmpty= "��� �������",
    infoFiltered= "(������������� �� _MAX_ �������)",
    infoPostFix= "",
    loadingRecords= "�������� �������...",
    zeroRecords= "������ �����������.",
    emptyTable= "� ������� ����������� ������",
    paginate=list(
      first= "������",
      previous= "����������",
      "next"= "���������",
      last= "�����"),
    aria=list(
      sortAscending= ": ������������ ��� ���������� ������� �� �����������",
      sortDescending= ": ������������ ��� ���������� ������� �� ��������")
  ))
}

dt_option_list=function(page_l=-1){
  list(language=DT_RU_LOCALE(),
                    dom = 'Blfrtip',
                    pageLength = page_l,
                    lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                    searching = TRUE,
                    lengthChange = TRUE,
                    scrollX = TRUE ,
                    scrollY=T,
                    buttons = c('copy', 'csv', 'excel')
  )
       }


DropDownMenuItem=function(mi,tabName,selected=FALSE) {
  if(mi$children[[1]]$attribs$href=="#"){
    mi$children[[1]]$attribs$href=paste0("#shiny-tab-",tabName)
  }
  
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(selected){
    mi$children[[1]]$attribs['data-start-selected']=1
  }
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


saveData=function(dd){
  saveRDS(dd,"data.rds")
}

loadData=function(){
  if(file.exists("data.rds"))
  readRDS("data.rds")
}

connect_to_db=function(db_path,pass){
  tryCatch(expr = {
    return (odbcConnectAccess2007(db_path,pwd = pass))},
    error = function(e) {
      return(e)
    })
}


get_years=function(db_path,pass){
  con=connect_to_db(db_path,pass)
  dat=sqlQuery(con,"select distinct YEAR(date) as year from Operations where (Operations.opertype=2)")
  odbcClose(con)
  return(dat)
}

get_season_data=function(db_path,pass,ses){
  con=connect_to_db(db_path,pass)
  sql=paste0(
    "select 
Month(date) as mm,
    sum(Operations.pricein*qtty ) as \"�����_�������\"    ,
    sum(qtty) as sum_qt,
    sum(Operations.priceout*qtty ) as \"�����_�������\",
    IIF(ISNULL(parent_gr.Name),'_',parent_gr.Name)  as parent_gr
    from (((((Operations
    inner join     operationtype on  (operationtype.id=Operations.opertype ) )
    inner join     goods on  (goods.id=Operations.goodid ) )
    inner join     partners on  (partners.id=Operations.partnerid ))
    left join     partnersgroups on  (partnersgroups.id=partners.groupid ))
    left join     goodsgroups on  (goodsgroups.id=goods.groupid ) )
    left join     goodsgroups as parent_gr on  (parent_gr.code=Left ( goodsgroups.Code, 3 ))
    where (Operations.opertype=2 and year(date)=",ses,")
    group by  IIF(ISNULL(parent_gr.Name),'_',parent_gr.Name) ,  Month(date)
    "
  )
  dat=sqlQuery(con,sql)
  
  
  odbcClose(con)
  return(dat)
}



get_data_for_pivot_season=function(db_path,pass,ses){
  con=connect_to_db(db_path,pass)
  sql=paste0(
    "select 
    Acct ,
    Month(date) as mm,
    date as date_,
    Operations.pricein ,
    Operations.pricein*qtty  as \"�����_�������\"    ,
    qtty as sum_qt,
    Operations.priceout,
    Operations.priceout*qtty  as \"�����_�������\",
    IIF(ISNULL(parent_gr.Name),'_',parent_gr.Name)  as parent_gr
    from (((((Operations
    inner join     operationtype on  (operationtype.id=Operations.opertype ) )
    inner join     goods on  (goods.id=Operations.goodid ) )
    inner join     partners on  (partners.id=Operations.partnerid ))
    left join     partnersgroups on  (partnersgroups.id=partners.groupid ))
    left join     goodsgroups on  (goodsgroups.id=goods.groupid ) )
    left join     goodsgroups as parent_gr on  (parent_gr.code=Left ( goodsgroups.Code, 3 ))
    where (Operations.opertype=2 and year(date)=",ses,")

    "
  )
  dat=sqlQuery(con,sql)
  dat_gr=dat%>%summarise(CNT_=sum(sum_qt),TOTAL_SUM_OUT=sum(`�����_�������`),
                         TOTAL_SUM_IN=sum(`�����_�������`),
                         CNT_CHEQ=n_distinct(Acct))
  
  dat_gr_ch=dat%>%group_by(Acct)%>%summarise(sum_OUT=sum(`�����_�������`), sum_IN=sum(`�����_�������`) , CNT_ch=sum(sum_qt))%>%
    summarise(AVG_OUT=mean(sum_OUT),AVG_IN=mean(sum_IN) , AVG_CNT=mean(CNT_ch))
  odbcClose(con)
  
  dat_itog=data.frame("������� ������"=dat_gr$CNT_ , "����� ������"=dat_gr$TOTAL_SUM_OUT,
                      "���������� ����"=dat_gr$TOTAL_SUM_IN ,
                      "���������� �����"=dat_gr$CNT_CHEQ,
                      "������� ���"=dat_gr_ch$AVG_OUT,
                      "������� ���������� ���� ����"=dat_gr_ch$AVG_IN,
                      "������� ������ ����"=dat_gr_ch$AVG_CNT)
 
  return( reshape2::melt(dat_itog,measure.vars=colnames(dat_itog)) )
}
get_oper_data_filters=function(db_path,pass){
  con=connect_to_db(db_path,pass)
  sql=paste0(
    "select 
    goods.name2 as \"�����\",
    partners.company2 as \"�������\",
    qtty as \"����������\",
    Operations.pricein as \"����_�������\",
    Operations.priceout as \"����_�������\",
    Operations.priceout-Operations.pricein as \"��������\",
    qtty*Operations.pricein  as \"���������_�������\",
    qtty*Operations.priceout as \"���������_�������\",
    qtty*(Operations.priceout-Operations.pricein)  as \"����������_���������\",
    partnersgroups.Name as \"���_��������\"   ,
    goodsgroups.Name as  \"������_�����������\"  ,
    parent_gr.Name  as  \"������\",
    [date] as  \"����\"
    from (((((Operations
    inner join     operationtype on  (operationtype.id=Operations.opertype ) )
    inner join     goods on  (goods.id=Operations.goodid ) )
    inner join     partners on  (partners.id=Operations.partnerid ))
    left join     partnersgroups on  (partnersgroups.id=partners.groupid ))
    left join     goodsgroups on  (goodsgroups.id=goods.groupid ) )
    left join     goodsgroups as parent_gr on  (parent_gr.code=Left ( goodsgroups.Code, 3 ))
    where (Operations.opertype=2 )
    "
  )
  dat=sqlQuery(con,sql)
  odbcClose(con)
  return(dat)
  
  
}

create_numeric_ui=function(title,id){
  fluidRow(id=paste0("row_",id),column(4,h4(title)),
    column(4,numericInput(inputId = paste0("low_",id),label = "��",value="")),
    column(4,numericInput(inputId = paste0("top_",id),label = "��",value="")))
  
}

create_character_ui=function(title,id,ch){
  fluidRow(id=paste0("row_",id),column(2,h4(title)),
           column(2,selectInput(inputId = paste0("type_",id),label = "",choices = c("� ������","�� � ������"))),
           column(8,selectInput(inputId = paste0("list_",id),label = "",choices = ch,multiple = T)))
  
}
