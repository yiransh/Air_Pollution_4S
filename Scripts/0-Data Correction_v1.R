# STROKE DATA =================================================================

## DATA BY ROWS (IDS) ---------------------------------------------------------
## highlighted in both Stroke_4S & Stroke_4S_final

## > stroke$ID[is.na(stroke$lkw_dt)]
## [1] "D8D56C7A10264473920D2AAF7F38F551" "D20E7B887D3EAF92E050007F01008D68"
## [3] "96F8BD7358834E6F846F707D5D89A414"

## [1] "D8D56C7A10264473920D2AAF7F38F551" & its following row
##      containing 151 cases: all cases saved as Stroke_4S_supplement
## [2] "D20E7B887D3EAF92E050007F01008D68" [3] "96F8BD7358834E6F846F707D5D89A414"
##      residential address occupying 2 columns and all data rightly shifted

## DATA BY VARs ---------------------------------------------------------------
### DATE AND TIME -----
### onset_dt failed to parse: 10·7-04-09 20:00 -----
stroke$onset_dt[which(stroke$ID == "F4CC61A91EEC41E1B95FB0F94CF0BEE7")]
stroke$onset_dt[which(stroke$ID == "F4CC61A91EEC41E1B95FB0F94CF0BEE7")] <- ymd_hm("2017-04-09 20:00")

### onset_dt is later than admission_date -----
### UNSURE CORRECTION: CODES ENDED WITH "#"
### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) == -1
adm_minus_vec1 <- ((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) == -1) 
                   & (date(stroke$lkw_dt) == date(stroke$onset_dt)))
stroke$admission_dt[which(adm_minus_vec1)] <- stroke$lkw_dt[which(adm_minus_vec1)]
adm_minus_vec2 <- ((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) == -1) 
                   & (date(stroke$lkw_dt) == date(stroke$admission_dt)))
stroke$admission_dt[which(adm_minus_vec2)] <- stroke$lkw_dt[which(adm_minus_vec2)]

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -300
View(stroke[which(date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -300), 
                         c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])   # 12 CASES
stroke$onset_dt[which(stroke$ID == "1613D01254254FCF9785828D5502B8E8")] <- ymd_hms("2017-05-03 10:00:00")
stroke$onset_dt[which(stroke$ID == "8F8136605F2445EBB3F637C7989A518E")] <- ymd_hms("2017-05-10 16:30:00")
stroke$onset_dt[which(stroke$ID == "89A946BA06574F30B174E6845E459838")] <- ymd_hms("2017-02-25 12:45:00")
stroke$onset_dt[which(stroke$ID == "750CEB8C9C494430AB6322B6D0029CD2")] <- ymd_hms("2017-01-31 14:00:00")
stroke$admission_dt[which(stroke$ID == "C6D008AA629E4ADA914E562DC11F995B")] <- ymd_hms("2017-12-18 05:20:00")
stroke$admission_dt[which(stroke$ID == "0F7C0FE35E474FBDAF94B2ECA63A82D0")] <- ymd_hms("2019-07-18 07:27:00")
stroke$onset_dt[which(stroke$ID == "7F78CFD114228236E050010A050263F0")] <- ymd_hms("2018-04-28 16:00:00")
stroke$admission_dt[which(stroke$ID == "CC8A959606AC4A69A2E693ACD7444F9C")] <- ymd_hms("2019-03-20 00:00:00")
stroke$admission_dt[which(stroke$ID == "85FB524A563346C9AAE9203FBB7265A1")] <- ymd_hms("2020-01-01 05:50:00")
stroke$onset_dt[which(stroke$ID == "1F975A26515148CEAB28C33DD3C6AD29")] <- ymd_hms("2019-12-09 05:00:00")
stroke$admission_dt[which(stroke$ID == "5861AE1214D9460F9D862FB110EA4AF7")] <- ymd_hms("2020-01-02 13:30:00")
stroke$onset_dt[which(stroke$ID == "5FE337C3E3744F788EF355243F7E2376")] <- ymd_hms("2020-12-13 11:10:00")

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- (-300, -26)
View(stroke[which(date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -26), 
              c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])     # 8 cases, 3 unsolved
stroke$admission_dt[which(stroke$ID == "F7EE88FCC2924996B2FDA85345133A32")] <- ymd_hms("2017-04-05 09:00:00")
stroke$admission_dt[which(stroke$ID == "D79FC4BA4CA440BC933B7742AEE6C7C9")] <- ymd_hms("2017-04-27 09:30:00")
stroke$admission_dt[which(stroke$ID == "7F7A38E102B5470DE050010A05026CEE")] <- ymd_hms("2018-12-12 00:00:00")
stroke$onset_dt[which(stroke$ID == "CE5F04ADD4E4D7E9E050007F0100308E")] <- ymd_hms("2021-10-03 10:30:00")
stroke$onset_dt[which(stroke$ID == "CDE5FFD03D65AD2AE050007F01006F0A")] <- ymd_hms("2021-06-27 08:00:00")

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-26, -14)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -26) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -14)), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])      # 10 cases, 6 unsolved
stroke$onset_dt[which(stroke$ID == "A1A93AE83E78433B93A11D4F1AAE088A")] <- ymd_hms("2017-01-01 20:00:00")
stroke$admission_dt[which(stroke$ID == "7F78CFD113C18236E050010A050263F0")] <- ymd_hms("2018-05-26 00:02:00")
# stroke$admission_dt[which(stroke$ID == "723FF604D4C54866A634338DF11D190C")] <- ymd_hms("2019-03-12 10:00:00")  ##2019-02-22
# stroke$admission_dt[which(stroke$ID == "D5C300A4A84639D6E050007F0100AD8D")] <- ymd_hms("2021-12-08 20:00:00")  ##2021-11-18

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-14, -11)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -14) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -11)), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])     # 11 cases, 10 unsolved
stroke$admission_dt[which(stroke$ID == "DE61B0141E5742EF8724DC3F9E40D35E")] <- ymd_hms("2018-02-28 09:30:00")

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-11, -6)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -11) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -6)), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])     # 29 cases, 4 unsolved
# stroke$admission_dt[which(stroke$ID == "F09C0BFB1AFF404786946EED7B7F4433")] <- ymd_hms("2017-10-20 08:50:00") #2017-10-10
# stroke$admission_dt[which(stroke$ID == "3893E971913B4328A17AFB9481383F01")] <- ymd_hms("2017-08-25 18:00:00") #2017-08-15
stroke$admission_dt[which(stroke$ID == "93660F52065F40BF9E41BBC7FC967D05")] <- ymd_hms("2017-03-30 20:39:00")
# stroke$admission_dt[which(stroke$ID == "BAFB105896024BAABA7AEDAD8F6B1A41")] <- ymd_hms("2018-02-14 08:00:00") #2018-02-04
# stroke$admission_dt[which(stroke$ID == "6FC7B1F960DA4AFFA285F6A4E7C7F2B8")] <- ymd_hms("2018-03-20 08:00:00") #2018-03-10
# stroke$admission_dt[which(stroke$ID == "D0B007E196B84E949BEE5A340F0A840F")] <- ymd_hms("2018-05-02 08:00:00") ##2018-04-20
stroke$admission_dt[which(stroke$ID == "B02E1EE5DF3148BB80318ABFB07A7B4A")] <- ymd_hms("2018-09-14 11:25:00")
stroke$admission_dt[which(stroke$ID == "7F78CFD1112E8236E050010A050263F0")] <- ymd_hms("2018-08-29 00:00:00")
stroke$admission_dt[which(stroke$ID == "B10D84FCDA7E414E85C9500E6B523DC6")] <- ymd_hms("2018-05-19 12:00:00")
# stroke$admission_dt[which(stroke$ID == "3992ACE144A64DFAAC36D25DF7D6077E")] <- ymd_hms("2019-10-04 08:00:00") ##2019-09-24
# stroke$admission_dt[which(stroke$ID == "EEEEDD494F2D4FD8B1B4169F09A6DA2A")] <- ymd_hms("2019-09-20 08:00:00") #2019-09-10
# stroke$admission_dt[which(stroke$ID == "4F2A0F5B5C434BA3A13E1639D755D4B9")] <- ymd_hms("2019-03-26 12:01:00") #2019-03-16
# stroke$admission_dt[which(stroke$ID == "94A4F4BA687248B6B5AFB1C2F2D1FDF0")] <- ymd_hms("2019-04-25 11:18:00") #2019-04-15
stroke$admission_dt[which(stroke$ID == "DB6AFEA54232457FABF18A0EA44FF1F7")] <- ymd_hms("2019-07-29 12:59:00")
stroke$onset_dt[which(stroke$ID == "5C198F5EE3484A3086B023E2AA69BEA3")] <- ymd_hms("2020-04-12 07:00:00")
stroke$admission_dt[which(stroke$ID == "47F17580E6B648E2B5E8D07E1552A6C8")] <- ymd_hms("2020-11-29 08:00:00")
# stroke$admission_dt[which(stroke$ID == "F52539AF084B44B39DA4C66876D804A5")] <- ymd_hms("2020-01-02 08:00:00") ##2020-12-25
# stroke$admission_dt[which(stroke$ID == "C56925C1950AC6E2E050007F010074D2")] <- ymd_hms("2020-08-09 13:00:00") ##2020-07-29
stroke$onset_dt[which(stroke$ID == "28A79E9540634DF18079569BB1102391")] <- ymd_hms("2020-12-04 07:42:00")
# stroke$admission_dt[which(stroke$ID == "3E4ECB4959844FB6813A0784899B4A6B")] <- ymd_hms("2020-08-19 13:30:00") #2020-08-12
stroke$admission_dt[which(stroke$ID == "D50C276625082542E050007F0100ADA7")] <- ymd_hms("2021-12-22 14:03:00")
stroke$admission_dt[which(stroke$ID == "CC8C3AD45146E5A9E050007F0100071E")] <- ymd_hms("2021-08-16 08:45:00")
stroke$admission_dt[which(stroke$ID == "93D77FAC1A804F579E198CCCD56DDBBA")] <- ymd_hms("2021-03-24 21:47:00")
# stroke$admission_dt[which(stroke$ID == "CB3AEFE56D4F0C15E050007F01000624")] <- ymd_hms("2021-08-28 11:12:00") ##2021-08-14
stroke$admission_dt[which(stroke$ID == "DAB602E453D192E8E050A8C06B0D36B2")] <- ymd_hms("2021-12-23 13:26:00")


### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-6, -3)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -6) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -3)), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])     # 36 cases, 4 unsolved
# stroke$admission_dt[which(stroke$ID == "B95A0982A8554039B7ADDC791E8A928C")] <- ymd_hms("2017-09-26 07:00:00") #2017-09-19
# stroke$admission_dt[which(stroke$ID == "28AAD850D2EF4FC4BA95BDD3C337597E")] <- ymd_hms("2017-09-25 07:30:00") #2017-09-18
stroke$onset_dt[which(stroke$ID == "963CD64498E24E9D954AF145A7070C8A")] <- ymd_hms("2017-01-11 12:00:00")
# stroke$admission_dt[which(stroke$ID == "15CEE44AB5FA478CA36464C50F3D7855")] <- ymd_hms("2017-07-20 08:00:00") ##2017-07-13
# stroke$admission_dt[which(stroke$ID == "20E85A342D934A38857A815F0740B914")] <- ymd_hms("2017-08-22 23:30:00") ##2017-08-16
# stroke$admission_dt[which(stroke$ID == "209D8E9C07F94A2EA73B35FD2E2BB186")] <- ymd_hms("2017-04-22 11:17:00") ##2017-04-15
# stroke$admission_dt[which(stroke$ID == "030F8437CEAB40F3B9CC3B43B7ABF670")] <- ymd_hms("2017-04-22 18:00:00") ##2017-04-15
# stroke$admission_dt[which(stroke$ID == "9C8D791466804399AF2837E8F983C15D")] <- ymd_hms("2017-08-30 09:00:00") ##2017-08-23
# stroke$admission_dt[which(stroke$ID == "735E0DD9BE8C4C8D87C33570DC90C864")] <- ymd_hms("2018-06-05 22:00:00") ##2018-05-29
# stroke$admission_dt[which(stroke$ID == "6326C0AA024643F69558D388A490A6AE")] <- ymd_hms("2018-05-09 10:00:00") ##2018-05-02
stroke$admission_dt[which(stroke$ID == "339939FD4AA241E6986AF6C1BBF82B8B")] <- ymd_hms("2018-10-18 13:45:00")
# stroke$admission_dt[which(stroke$ID == "EBFE38F166A6490B8F1026BF1C291081")] <- ymd_hms("2018-03-31 00:00:00") ##2018-03-24
# stroke$admission_dt[which(stroke$ID == "2A36DABD15924001A1D21E7264D19351")] <- ymd_hms("2018-04-20 16:00:00") ##2018-04-13
# stroke$admission_dt[which(stroke$ID == "919B6BA85DD841C5A69E52FB9200DD0E")] <- ymd_hms("2019-05-16 08:00:00") ##2019-05-09
stroke$admission_dt[which(stroke$ID == "AE62D287A6BB45DAA6A70DCEBCD5A0CB")] <- ymd_hms("2019-05-28 08:00:00")
# stroke$admission_dt[which(stroke$ID == "CBA4C21015A54B08B24B03C74887FBA0")] <- ymd_hms("2019-11-12 16:00:00") ##2019-11-05
# stroke$admission_dt[which(stroke$ID == "0E05771874E040DC9C92C999F7BFB0C8")] <- ymd_hms("2019-09-25 09:00:00") ##2019-09-18
# stroke$admission_dt[which(stroke$ID == "913AD7CF10C8416DB9C5FD9086261527")] <- ymd_hms("2019-11-14 19:02:00") ##2019-11-07
# stroke$admission_dt[which(stroke$ID == "4DF8F5C3B3354622902ADDCAEDD8BFB5")] <- ymd_hms("2019-10-18 00:00:00") ##2019-10-11
# stroke$admission_dt[which(stroke$ID == "D48DA7C864A342BA96AB6ED47C3D2C7C")] <- ymd_hms("2020-05-19 09:00:00") ##2020-05-12
# stroke$admission_dt[which(stroke$ID == "C8FB17B9DBB24231887C20086C0453C9")] <- ymd_hms("2020-04-06 09:00:00") ##2020-03-30
# stroke$admission_dt[which(stroke$ID == "4EA679E4FD52472C973D27F38E9FC4CD")] <- ymd_hms("2020-05-15 09:00:00") ##2020-05-08
# stroke$admission_dt[which(stroke$ID == "B507F1F777EA4B538A673C760130DA20")] <- ymd_hms("2020-07-11 19:30:00") ##2020-07-04
# stroke$admission_dt[which(stroke$ID == "26C49ABDDD014F1E965265A48F27DB16")] <- ymd_hms("2020-04-25 16:00:00") ##2020-04-18
# stroke$admission_dt[which(stroke$ID == "CBE393B96C1340A98C8D862DFB6938B0")] <- ymd_hms("2020-06-26 13:35:00") ##2020-06-19
# stroke$admission_dt[which(stroke$ID == "2B649F03A3264712AE962D15AE40B8D7")] <- ymd_hms("2020-08-08 05:40:00") ##2020-08-01
# stroke$admission_dt[which(stroke$ID == "3A4D8F0031A046CBBD7EA947C5ECFB8D")] <- ymd_hms("2020-05-26 09:14:00") ##2020-05-19
# stroke$admission_dt[which(stroke$ID == "CA377ECF8BFD6ED4E050007F01009055")] <- ymd_hms("2021-07-30 09:00:00") ##2021-07-23
# stroke$admission_dt[which(stroke$ID == "CAEA4E60F299B920E050007F010039A1")] <- ymd_hms("2021-05-26 08:00:00") ##2021-05-19
# stroke$admission_dt[which(stroke$ID == "CECA945E6B9991E1E050007F01003090")] <- ymd_hms("2021-08-30 10:00:00") ##2021-08-24
# stroke$admission_dt[which(stroke$ID == "D50AD823DFDBD65BE050007F0100ADA9")] <- ymd_hms("2021-11-17 10:00:00") ##2021-11-10
# stroke$admission_dt[which(stroke$ID == "327F0858D27743B3AE6C79E542CF5FCD")] <- ymd_hms("2021-01-17 14:00:00") #2021-01-11


### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-3, -2)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -3) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -2)), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])     # 26 cases, 4 unsolved
stroke$onset_dt[which(stroke$ID == "DC0D7160940A42289A0CAD83414D416B")] <- ymd_hms("2017-02-21 09:13:00")
# stroke$admission_dt[which(stroke$ID == "2BA535E2EF17463D8F973C9487163791")] <- ymd_hms("2017-06-14 10:00:00") ##2017-06-07
# stroke$admission_dt[which(stroke$ID == "D90C488FDD2F43DEBE2A04A4C77DC219")] <- ymd_hms("2017-11-10 08:00:00") ##2017-11-03
# stroke$admission_dt[which(stroke$ID == "8336F234A8124D66A49EB247FE3523CC")] <- ymd_hms("2018-01-24 10:00:00") ##2018-01-17
# stroke$admission_dt[which(stroke$ID == "1AD1DAF47CBC4C66AC270EFA4EBE958A")] <- ymd_hms("2018-09-19 11:00:00") ##2018-09-12
# stroke$admission_dt[which(stroke$ID == "7F78CFD112EA8236E050010A050263F0")] <- ymd_hms("2018-07-23 00:00:00") ##2018-07-05
# stroke$admission_dt[which(stroke$ID == "18E68F5CD40B49C8B75EA1C523813A3F")] <- ymd_hms("2018-05-21 12:00:00") ##2018-05-14
# stroke$admission_dt[which(stroke$ID == "A146E484E9C34EB4BBB64BFB8B5E510B")] <- ymd_hms("2018-08-11 07:30:00") ##2018-08-04
# stroke$admission_dt[which(stroke$ID == "5BC55F671D7A444F91D737D04735FC7E")] <- ymd_hms("2018-08-18 09:30:00") ##2018-08-11
# stroke$admission_dt[which(stroke$ID == "6FE4661EAFAF416B9B7D72896F869A3D")] <- ymd_hms("2019-07-31 08:00:00") ##2019-07-24
# stroke$admission_dt[which(stroke$ID == "998BD84D385B457C9AD899BD60A5381D")] <- ymd_hms("2019-09-17 00:30:00") ##2019-09-10
# stroke$admission_dt[which(stroke$ID == "0EC30FFAA20148C9B1AFEE029A626EDC")] <- ymd_hms("2019-10-28 15:00:00") ##2019-10-21
# stroke$admission_dt[which(stroke$ID == "2B86A3F6F38D4387B2263718E66A5A88")] <- ymd_hms("2019-12-23 16:55:00") ##2019-12-16
# stroke$admission_dt[which(stroke$ID == "590164FD774042D6A6994FF94F55612E")] <- ymd_hms("2020-12-29 09:00:00") ##2020-12-22
# stroke$admission_dt[which(stroke$ID == "AB0E73E858BB44A2B5DCDD951F768273")] <- ymd_hms("2020-12-19 16:30:00") ##2020-12-12
# stroke$admission_dt[which(stroke$ID == "DE7E9216D1114F4D818D51675F52E811")] <- ymd_hms("2020-01-09 11:00:00") ##2020-01-03
# stroke$admission_dt[which(stroke$ID == "7A73256AA19C4CA0A9F5CA93D29BAC6D")] <- ymd_hms("2020-08-15 23:10:00") ##2020-08-08
# stroke$admission_dt[which(stroke$ID == "C3C31309720235D4E050007F010064EA")] <- ymd_hms("2021-01-09 08:00:00") ##2021-01-02
# stroke$admission_dt[which(stroke$ID == "CB52250F1CB68EE5E050007F01005F06")] <- ymd_hms("2021-07-08 15:00:00") ##2021-07-01
#stroke$admission_dt[which(stroke$ID == "D1AEA2F576333ED9E050007F01008D72")] <- ymd_hms("2021-10-18 07:00:00") ##2021-10-11
stroke$admission_dt[which(stroke$ID == "D149981D9CD1BDB9E050007F01008C5A")] <- ymd_hms("2021-11-17 09:05:00")
# stroke$admission_dt[which(stroke$ID == "D5AFE32CBEBE43F3E050007F0100AD87")] <- ymd_hms("2021-11-13 09:30:00") ##2021-11-06

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-2, -1)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -2) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < -1)
                  & (date(stroke$onset_dt) != date(stroke$lkw_dt))), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])  
stroke$onset_dt[which(stroke$ID == "DE3397887A374DA098E70E7C33F6B64C")] <- ymd_hms("2017-03-15 01:00:00")
stroke$onset_dt[which(stroke$ID == "DB33433478A145EBB0B11F4CFE096585")] <- ymd_hms("2017-01-12 04:00:00")
stroke$admission_dt[which(stroke$ID == "E04FB3F8C85F4640AE297A0ED5053880")] <- ymd_hms("2020-04-13 15:33:00")
stroke$admission_dt[which(stroke$ID == "5D0C0EA8F85D4683933CE7CEDB764F95")] <- ymd_hms("2021-01-18 21:45:00")

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- [-1, 0)
View(stroke[which((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -1) 
                  & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < 0)
                  & (date(stroke$onset_dt) != date(stroke$lkw_dt))), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])  
onset_minus_vec1 <- ((date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) >= -1) 
                     & (date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) < 0)
                     & (date(stroke$onset_dt) != date(stroke$lkw_dt))
                     & (date(stroke$lkw_dt) == date(stroke$admission_dt))
                     & (hms(format(stroke$onset_dt, format = "%H:%M:%S")) >= hms(format(stroke$lkw_dt, format = "%H:%M:%S")))
                     & (hms(format(stroke$onset_dt, format = "%H:%M:%S")) <= hms(format(stroke$admission_dt, format = "%H:%M:%S"))))
date(stroke$onset_dt[which(onset_minus_vec1)]) <- date(stroke$lkw_dt[which(onset_minus_vec1)])

### date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) <<- (-300, 0)
View(stroke[which(date(stroke$onset_dt) %--% date(stroke$admission_dt)/ddays(1) > 300), 
            c("ID", "lkw_dt", "onset_dt", "admission_dt", "discharge_dt")])   # 10
stroke$admission_dt[which(stroke$ID == "")] <- ymd_hms("")

### FACTOR VARIABLE -----
### none

### NUMERIC VARIABLE -----
### decimals in discharge mRS
### inconsistent discharge mRS & dicharge mortality
