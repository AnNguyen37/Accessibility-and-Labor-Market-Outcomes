##### PREPARING DATA #####

rm(list = ls())
library(stargazer)
library(Hmisc)
library(tidyverse)
library(haven)
library(zoo)
library(dplyr)
# Set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/PhD/Accessibility and Labor Market Outcomes")

# Load commune name data
library(readxl)
commune_name <- tbl_df(read_dta("commune_name_2016.dta"))

# Convert from lowercase to uppercase
commune_name <- commune_name %>% mutate_if(is.character, str_to_title) 
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# panelid data and join with commune name data
panelid <- tbl_df(read_dta("panelid.dta")) %>% 
        left_join(commune_name, by = c("tinh_2016", "quan_2016", "xa_2016"))

# Rplace the NA base on group of tinh quan xa of previous year

panelid <- panelid %>% group_by(tinh_2008, quan_2008, xa_2008) %>% 
        mutate(across(c(starts_with("ten")), na.locf0))  %>% ungroup

panelid <- panelid %>% arrange(tinh_2008, quan_2008, xa_2008)

# Tidying ESA data --------------------------------------------
# Load dataset
esa <- tbl_df(read.csv("ESA.csv", stringsAsFactors = FALSE)) %>% 
        select(asdf_id, starts_with("NAME"), matches(".2007."), matches(".2009."),
               matches(".2011."), matches(".2013."), matches(".2015."))

names(esa)

# Tidying data
esa <- esa %>% 
        gather(key = "key", value = "cases", 
               contains("esa_landcover"),
               na.rm = FALSE
        )

esa <- esa %>% 
        separate(key, into = paste0('key', 1:3), sep = '[.]') %>% 
        select(-c(key1, NAME_0)) %>% 
        rename(year = key2) %>% 
        separate(key3, into = paste0('key', 1:3), sep = '[_]') %>% 
        select(-c(key1, key3))

esa <- esa %>% 
        pivot_wider(names_from = key2, values_from = cases) %>% 
        rename(c(tentinh = NAME_1,
                 tenquan = NAME_2,
                 tenxa = NAME_3))


# Correct tentinh tenquan tenxa in the two data

panelid <- panelid %>% 
        mutate(tentinh = str_replace_all(tentinh, c("Hà Tây" = "Hà Nội",
                                                    "Ha Nội" = "Hà Nội",
                                                    "Lao Cai" = "Lào Cai",
                                                    "Phu Tho" = "Phú Thọ",
                                                    "Phú Thok" = "Phú Thọ",
                                                    "Lai Chau" = "Lai Châu",
                                                    "Dien Bien"= "Điện Biên",
                                                    "Quang Nam" = "Quảng Nam",
                                                    "Quảng Nam' Nông S" = "Quảng Nam",
                                                    "Qua?Ng Nam	" = "Quảng Nam",
                                                    "Khánh Hòa" = "Khánh Hòa",
                                                    "Khanh Hoa" = "Khánh Hòa",
                                                    "Đăk Lăk" = "Đắk Lắk",
                                                    "Đắk Nông" = "Đăk Nông",
                                                    "Dac Nong" = "Đăk Nông",
                                                    "Đak Nông" = "Đăk Nông",
                                                    "Dak Lak" = "Đắk Lắk",
                                                    "Điên Biên" = "Điện Biên")))

# Ha Noi ------------------------------------------------------------------

panelid[which(panelid$tentinh == "Hà Nội"),] <- panelid[which(panelid$tentinh == "Hà Nội"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Ứng Hoà" = "Ứng Hòa",
                                                    "Phu Xuyên" = "Phú Xuyên"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Phụng Thương" = "Phụng Thượng",
                                                "Dị Nâu" = "Dị Nậu",
                                                "Quảng Bi" = "Quảng Bị",
                                                "Thư Phúc" = "Thư Phú",
                                                "Hoà Xá" = "Hòa Xá",
                                                "Bài Lâm Thượng" = "Hồng Quang",
                                                "Sơn Ha" = "Sơn Hà",
                                                "Phu Túc" = "Phú Túc")))

esa[which(esa$tentinh == "Hà Nội"),] <- esa[which(esa$tentinh == "Hà Nội"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Ba Vi" = "Ba Vì",
                                                    "Ứng Hoà" = "Ứng Hòa"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Thái Hòa" = "Thái Hoà",
                                                "V¨n Vâ" = "Văn Võ",
                                                "Thánh Mai" = "Thanh Mai",
                                                "Lê Thánh" = "Lê Thanh",
                                                "Ân Phú" = "An Phú")))


# Lào Cai -----------------------------------------------------------------

panelid[which(panelid$tentinh == "Lào Cai"),] <- panelid[which(panelid$tentinh == "Lào Cai"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Vân Bàn" = "Văn Bàn",
                                                    "Tp Lào Cai" = "Lào Cai",
                                                    "Sima Cai" = "Si Ma Cai"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("P Bình Minh" = "Bình Minh",
                                                "Bản Xen" = "Bản Sen",
                                                "Càn Cấu" = "Cán Cấu")))

esa[which(esa$tentinh == "Lào Cai"),] <- esa[which(esa$tentinh == "Lào Cai"),] %>%
        mutate(tenxa = str_replace_all(tenxa, c("Vâ Lao" = "Võ Lao",
                                                "Kh¸nh Yên H¹" = "Khánh Yên Hạ",
                                                "Liêm Phó" = "Liêm Phú")))


# Phú Thọ -----------------------------------------------------------------

panelid[which(panelid$tentinh == "Phú Thọ"),] <- panelid[which(panelid$tentinh == "Phú Thọ"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Tp Việt Trì" = "Việt Trì",
                                                    "Tx Phú Thọ" = "Phú Thọ",
                                                    "Doan Hung" = "Đoan Hùng",
                                                    "Hạ Hòa" = "Hạ Hoà",
                                                    "Phú Ninh" = "Phù Ninh",
                                                    "Lam Thao" = "Lâm Thao",
                                                    "Yen Lap" = "Yên Lập",
                                                    "Tam Nong" = "Tam Nông",
                                                    "Thanh Thủy" = "Thanh Thuỷ",
                                                    "Tan Son" = "Tân Sơn"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Hung Quan" = "Hùng Quan",
                                                "Chí Đán" = "Chí Đám",
                                                "Đan Hạ" = "Đan Hà",
                                                "Yên Khê" = "Yển Khê",
                                                "Liên Hoa" = "Liên Hoà",
                                                "Tien Kien" = "Tiên Kiên",
                                                "Sơn Vy" = "Sơn Vi",
                                                "Thương Long" = "Thượng Long",
                                                "Ngoc Lap" = "Ngọc Lập",
                                                "Hung Do" = "Hùng Đô",
                                                "Đông Cứu"  = "Đông Cửu",
                                                "Minh Dai" = "Minh Đài"))) %>% 
        mutate(tenxa = ifelse(tenxa == "Vân Lĩnh (Thanh Vân)", "Thanh Vân", tenxa))


esa[which(esa$tentinh == "Phú Thọ"),] <- esa[which(esa$tentinh == "Phú Thọ"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Phú Ninh" = "Phù Ninh"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Văn Lung" = "Văn Lang",
                                                "Thánh Vân" = "Thanh Vân",
                                                "Minh Đµi" = "Minh Đài",
                                                "Lai Đång" = "Lai Đồng"))) %>% 
        mutate(tenquan= ifelse(tenquan == "Phù Ninh" & tenxa == "Kim Đức", "Việt Trì", tenquan))  %>% 
        mutate(tenquan = ifelse(tenquan == "Thanh Sơn" & tenxa == "Minh Đài", "Tân Sơn", tenquan)) %>% 
        mutate(tenxa = ifelse(asdf_id == 7232, "Kim Thượng", tenxa)) %>% 
        mutate(tenquan = ifelse(asdf_id == 7232 | asdf_id == 7234, "Tân Sơn", tenquan))



# Lai Châu ----------------------------------------------------------------

panelid[which(panelid$tentinh == "Lai Châu"),] <- panelid[which(panelid$tentinh == "Lai Châu"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Muong Te" = "Mường Tè",
                                                    "Phong Tho" = "Phong Thổ",
                                                    "Tam Duong" = "Tam Đường",
                                                    "Sim Ho" = "Sìn Hồ",
                                                    "Sin Ho" = "Sìn Hồ",
                                                    "Tan Uyen" = "Tân Uyên",
                                                    "Than Uyen" = "Thanh Uyên",
                                                    "Tx Lai Châu" = "Lai Châu"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Nam Khao" = "Nậm Khao",
                                                "Dao Sam" = "Dào San",
                                                "Khổng Lác" = "Khổng Lào",
                                                "Thèn Sìn" = "Thèn Xin",
                                                "Ta Leng" = "Tả Lèng",
                                                "Nung Nay" = "Nùng Nàng",
                                                "Le Loi" = "Lê Lợi",
                                                "Pac Ta" = "Pắc Ta",
                                                "Muong Kim" = "Mường Kim",
                                                "San Thảng" = "San Thàng",
                                                "Nậm Loỏng" = "Nậm Luông",
                                                "Vàng San" = "Bum Nưa",
                                                "Lùng Thàng" = "Ma Quai",
                                                "Phuc Than" = "Mường Than",
                                                "Muong Cang" = "Nµ Cang")))

esa[which(esa$tentinh == "Lai Châu"),] <- esa[which(esa$tentinh == "Lai Châu"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Thanh Uyen" = "Thanh Uyên"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Huæi Luông" = "Huổi Luông"))) %>% 
        mutate(tenquan = ifelse(asdf_id == 5667, "Phong Thổ", tenquan)) %>% 
        mutate(tenquan = ifelse(asdf_id == 5717 | asdf_id == 5711, "Tân Uyên", tenquan))

# Điện Biên	 ---------------------------------------------------------------

panelid[which(panelid$tentinh == "Điện Biên"),] <- panelid[which(panelid$tentinh == "Điện Biên"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Thanh Pho Dien Bien Phu" = "Điên Biên Phủ",
                                                    "Điện Biên Đông]" = "Điện Biên Đông",
                                                    "Điên Biên Đông" = "Điện Biên Đông",
                                                    "Dien Bien" = "Điện Biên",
                                                    "Tùa Chùa" = "Tủa Chùa",
                                                    "Tuấn Giáo" = "Tuần Giáo"))) %>%
        mutate(tenxa = str_replace_all(tenxa, c("Tùa Thàng" = "Tủa Thàng",
                                                "Lao Xả Phình" = "Lao Xả Phình",
                                                "Nà Sáng" = "Nà Sáy",
                                                "Muong Luan" = "Mường Luân",
                                                "Rạng Đông" = "Phình Sáng",
                                                "Hẹ Muụng" = "Núa Ngam",
                                                "Mường Lạn" = "Mường Lạn"))) %>% 
        mutate(tenquan = ifelse(tenxa == "Mường Luân", "Điện Biên Đông", tenquan))
        
        
esa[which(esa$tentinh == "Điện Biên"),] <- esa[which(esa$tentinh == "Điện Biên"),] %>%
        mutate(tenxa = str_replace_all(tenxa, c("Thánh Minh" = "Thanh Minh",
                                                "Thánh Xương" = "Thanh Xương",
                                                "Thánh Yên" = "Thanh Yên"))) %>% 
        mutate(tenquan = ifelse(asdf_id == 661, "Mường Ẳng", tenquan))


# Nghệ An -----------------------------------------------------------------

panelid <- panelid %>% 
        mutate(tentinh = str_replace_all(tentinh, c("Nghê An" = "Nghệ An")))
        	
panelid[which(panelid$tentinh == "Nghệ An"),] <- panelid[which(panelid$tentinh == "Nghệ An"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Thị Xã Hoàng Mai" = "Hoàng Mai",
                                                    "Quỳnh Lưu- Hoàng Mai" = "Hoàng Mai",
                                                    "Quynh Lưu" = "Quỳnh Lưu",
                                                    "Nghi Lôc" = "Nghi Lộc"))) %>% 
        mutate(tenquan = ifelse(tenxa == "Tân Sơn", "Quỳnh Lưu", tenquan)) %>%
        mutate(tenxa = str_replace_all(tenxa, c("Quynh Mỹ" = "Quỳnh Mỹ",
                                                "Phá Đánh" = "Phà Đánh",
                                                "Nhgi Thái" = "Nghi Thái",
                                                "Cát Văn" = "Cát Vân",
                                                "Hưng Phu" = "Hưng Phú",
                                                "Viễn Lâm" = "Diễn Lâm")))

esa[which(esa$tentinh == "Nghệ An"),] <- esa[which(esa$tentinh == "Nghệ An"),] %>%
        mutate(tenxa = str_replace_all(tenxa, c("Mường Noọc" = "Mường Nọc",
                                                "Đồng Hopự" = "Đồng Hợp",
                                                "Yên Hoà" = "Yên Hòa",
                                                "Quỳnh Ngọ" = "Quỳnh Ngọc",
                                                "Thánh My" = "Thanh Mỹ",
                                                "Thánh Vân" = "Thanh Văn",
                                                "Thánh Khai" = "Thanh Khai",
                                                "Thánh Lâm" = "Thanh Lâm"))) %>% 
        mutate(tenquan = ifelse(asdf_id == 6649 | asdf_id == 6658 | asdf_id == 6668, "Hoàng Mai", tenquan)) 


# Quảng Nam ---------------------------------------------------------------

panelid <- panelid %>% 
        mutate(tentinh = ifelse(tenxa == "Duy Trinh", "Quảng Nam", tentinh)) %>% 
        mutate(tentinh = str_replace_all(tentinh, c("Qaung Nam" = "Quảng Nam",
                                                    "Quang  Nam" = "Quảng Nam")))
        

panelid[which(panelid$tentinh == "Quảng Nam"),] <- panelid[which(panelid$tentinh == "Quảng Nam"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Hooi An" = "Hội An",
                                                    "Dai Loc" = "Đại Lộc",
                                                    "Điên Ban" = "Điện Bàn",
                                                    "Dien Bien" = "Điện Bàn",
                                                    "Dien Ban" = "Điện Bàn",
                                                    "Duy Xuyen" = "Duy Xuyên",
                                                    "Thang Binh" = "Thăng Bình",
                                                    "Que Son" = "Quế Sơn",
                                                    "Que Sơn" = "Quế Sơn",
                                                    "Hiêp Đưc" = "Hiệp Đức",
                                                    "Nui Thanh" = "Núi Thành",
                                                    "Nông Sơn" = "Quế Sơn",
                                                    "Nui Thanh" = "Núi Thành"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Câm Nam" = "Cẩm Nam",
                                                "Xã Ba" = "Ba",
                                                "Dai Quang" = "Đại Quang",
                                                "Dai Hiep" = "Đại Hiệp",
                                                "Dien Hong" = "Điện Hồng",
                                                "Điên Phươc" = "Điện Phước",
                                                "Điên Minh" = "Điện Minh",
                                                "Điện Thắng Bắc" = "Điện Thắng",
                                                "Dien Nam Trung" = "Điện Nam",
                                                "Duy Phuoc" = "Duy Phước",
                                                "Binh Phucq" = "Bình Phúc",
                                                "Bình Định Bắc" = "Bình Định",
                                                "Binh Trung" = "Bình Trưng",
                                                "Quê Cương" = "Quế Cường",
                                                "Quôc Binh" = "Quế Bình",
                                                "Tam Xuân 2" = "Tam Xuân II",
                                                "Tam Anh Bắc" = "Tam Anh",
                                                "Tam Anh Nam" = "Tam Anh",
                                                "Tam Hiêp" = "Tam Hiệp",
                                                "Tam My Tây" = "Tam Mỹ",
                                                "Phú Thịnh" = "Tam Vinh",
                                                "Đại An" = "Đại Hòa")))

esa[which(esa$tentinh == "Quảng Nam"),] <- esa[which(esa$tentinh == "Quảng Nam"),] %>%
        mutate(tenxa = str_replace_all(tenxa, c("ĐiÖn Nam" = "Điện Nam",
                                                "Tam Mü" = "Tam Mỹ")))


# Khánh Hòa ---------------------------------------------------------------

panelid <- panelid %>% 
        mutate(tentinh = str_replace_all(tentinh, c("Khánh Hoà" = "Khánh Hòa",
                                                    "Khanh Hòa" = "Khánh Hòa"))) 

panelid[which(panelid$tentinh == "Khánh Hòa"),] <- panelid[which(panelid$tentinh == "Khánh Hòa"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Tp Nha Trang" = "Nha Trang",
                                                    "Vsnj Ninh" = "Vạn Ninh",
                                                    "Diên Khanh" = "Diên Khánh"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Vĩnh Hương" = "Vĩnh Phương",
                                                "Vĩnh Thanh" = "Vĩnh Thạnh",
                                                "Vạn  Hưng" = "Vạn Hưng",
                                                "Ninh Tho" = "Ninh Thọ",
                                                "Tt Cam Đức" = "Cam Đức")))

esa[which(esa$tentinh == "Khánh Hòa"),] <- esa[which(esa$tentinh == "Khánh Hòa"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Van Ninh" = "Vạn Ninh"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Ninh Ých" = "Ninh Ích",
                                                "Diên Thä" = "Diên Thọ",
                                                "Diên Phuíc" = "Diên Phước",
                                                "Suèi HiÖp" = "Suối Hiệp")))

# Đắk Lắk -----------------------------------------------------------------

panelid[which(panelid$tentinh == "Đắk Lắk"),] <- panelid[which(panelid$tentinh == "Đắk Lắk"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Buôn Mê Thuột" = "Buôn Ma Thuột",
                                                    "Krong Buk" = "Krông Buk",
                                                    "Tx. Buôn Hồ" = "Buôn Hồ",
                                                    "Tx Buôn Hồ" = "Buôn Hồ",
                                                    "Cư M Gar" = "Cư M'gar",
                                                    "Cư M'ga" = "Cư M'gar",
                                                    "Cư M'garr" = "Cư M'gar",
                                                    "M' Drắk" = "M'Đrăk",
                                                    "Krông Păk" = "Krông Pắk",
                                                    "Krông Păc" = "Krông Pắk",
                                                    "Krong Păk" = "Krông Pắk",
                                                    "Krong Pắk" = "Krông Pắk",
                                                    "Krông Pắc" = "Krông Pắk",
                                                    "Cư Krim" = "Cư Kuin",
                                                    "Lăk" = "Lắk"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Cư E Buôr" = "Cưebua",
                                                "Hòa Thắng" = "Hoà Thắng",
                                                "Hòa Khánh" = "Hoà Khánh",
                                                "Ea Rok" = "Ea Rốc",
                                                "Ea Toh" = "Ea Tóh",
                                                "Chư Kbo" = "Chư KBô",
                                                "Cuôr Knia" = "Cuôr KNia",
                                                "Ea M'droh" = "Ea M'Dróh",
                                                "Ea Drơng" = "Ea DRơng",
                                                "Ea Tyh" = "Ea Tíh",
                                                "Cư Króa" = "Cư Kroá",
                                                "Đắc Phơi" = "Dắk Phơi")))

esa[which(esa$tentinh == "Đắk Lắk"),] <- esa[which(esa$tentinh == "Đắk Lắk"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Buon Ma Thuot" = "Buôn Ma Thuột",
                                                    "Lăk" = "Lắk"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Chứ Kbô" = "Chư KBô",
                                                "Ea KnuÕc" = "Ea Kuang",
                                                "Vô Bæn" = "Vụ Bốn"))) %>% 
        mutate(tenquan = ifelse(asdf_id == 534 | asdf_id == 523, "Buôn Hồ", tenquan)) %>% 
        mutate(tenquan = ifelse(asdf_id == 505 | asdf_id == 502, "Cư Kuin", tenquan)) 


# Đăk Nông ----------------------------------------------------------------

panelid <- panelid %>% 
        mutate(tentinh = str_replace_all(tentinh, c("Dak Nong" = "Đăk Nông",
                                                    "Đăc Nông" = "Đăk Nông"))) %>% 
        mutate(tentinh = ifelse(tenxa == "Dak Hoa", "Đăk Nông", tentinh))

panelid[which(panelid$tentinh == "Đăk Nông"),] <- panelid[which(panelid$tentinh == "Đăk Nông"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Cư Jut" = "Cư Jút",
                                                    "Cư Rút" = "Cư Jút",
                                                    "Co Jut" = "Cư Jút",
                                                    "Co Juk" = "Cư Jút",
                                                    "Krông Nô" = "Krông Nô",
                                                    "Krong Nô" = "Krông Nô",
                                                    "Krong No" = "Krông Nô",
                                                    "Dak Mil" = "Đăk Mil",
                                                    "Dak Song" = "Đăk Song",
                                                    "Đắc R' Lấp" = "Đăk R'Lấp",
                                                    "Đak Glong" = "Đăk Glong",
                                                    "Dac Rlap" = "Đăk R'Lấp"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Eapo" = "Ea Pô",
                                                "Nam Dong" = "Nam Đồng",
                                                "Đắk Drông" = "Đăk Đrông",
                                                "Tan Thang" = "Tâm Thắng",
                                                "Trung Son" = "Trúc Sơn",
                                                "Nam Xuân" = "Đăk Sôr",
                                                "Dak Dro" = "Đăk Rô",
                                                "Đăk Lao" = "Đắk Lao",
                                                "Dak Sak" = "Đắk Sắk",
                                                "Đăk Minh" = "Đức Minh",
                                                "Dak Hoa" = "Đắk Môl",
                                                "Đak Hòa" = "Đắk Môl",
                                                "Thuận Hanh" = "Thuận Hạnh",
                                                "Nam Jang" = "Đắk N'Drung",
                                                "Nghĩa Thắng" = "Đạo Nghĩa",
                                                "Hung Binh" = "Đắk Sin",
                                                "Đak Ru" = "Đắk Ru",
                                                "Dac Wer" = "Nhân Cơ",
                                                "Quảng Trự" = "Quảng Trực",
                                                "Quảng Hòa" = "Quảng Sơn",
                                                "Đak Plao" = "Đăk PLao",
                                                "Nam Bình" = "Đắk Song",
                                                "Nghia Thang" = "Đạo Nghĩa")))

esa[which(esa$tentinh == "Đăk Nông"),] <- esa[which(esa$tentinh == "Đăk Nông"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Dak Song" = "Đăk Song")))


# Lâm Đồng ----------------------------------------------------------------

panelid <- panelid %>% 
        mutate(tentinh = str_replace_all(tentinh, c("Lâm Đông" = "Lâm Đồng")))

panelid[which(panelid$tentinh == "Lâm Đồng"),] <- panelid[which(panelid$tentinh == "Lâm Đồng"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Tp Đa Lat" = "Đà Lạt",
                                                    "Đươn Dương" = "Đơn Dương",
                                                    "Đưc Trong" = "Đức Trọng",
                                                    "Lâm Ha" = "Lâm Hà",
                                                    "Đa Te'h" = "Đạ Tẻh"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Xuân Trương" = "Xuân Trường",
                                                "Tt Lạc Dương" = "Lạc Dương",
                                                "Pro'" = "P'Róh",
                                                "Hiệp Thành" = "Hiệp Thạnh",
                                                "Bình Thạnh" = "Bình Thành",
                                                "Ta Hine" = "Tà Hine",
                                                "Loeen Sron'h" = "Liêng S’Rônh",
                                                "Tân Ha" = "Tân Hà",
                                                "Lôc Đưc" = "Lộc Đức",
                                                "Hoa Ninh" = "Hoà Ninh",
                                                "Hòa Bắc" = "Hoà Bắc",
                                                "Đạ Ploa" = "ĐạP'loa",
                                                "Đinh Trang Hòa" = "Đinh Trang Hoà")))


# Long An	 ----------------------------------------------------------------

panelid[which(panelid$tentinh == "Long An"),] <- panelid[which(panelid$tentinh == "Long An"),] %>%
        mutate(tenquan = str_replace_all(tenquan, c("Việt Hưng" = "Vĩnh Hưng",
                                                    "Mộc Hoa" = "Mộc Hóa",
                                                    "Tân Thạnh" = "Tân Thành",
                                                    "Tân Thanh" = "Tân Thành",
                                                    "Thạnh Hóa" = "Thanh Hóa",
                                                    "Thanh Hoá" = "Thanh Hóa",
                                                    "Châu Thnah" = "Châu Thành"))) %>% 
        mutate(tenxa = str_replace_all(tenxa, c("Hoàng Thọ Phú" = "Hướng Thọ Phú",
                                                "Khanh Hậu" = "Khánh Hậu",
                                                "Bình Hòa Bắc" = "Bình Hoà Bắc",
                                                "Hòa Khánh Nam" = "Hoà Khánh Nam",
                                                "Đức Hòa Thượng" = "Đức Hoà Thượng",
                                                "An Lục Long" = "An Lôc Long",
                                                "Nhứt Ninh" = "Nhựt Ninh",
                                                "Xã Long Định" = "Long Định",
                                                "Tân Khánh" = "Khánh Hậu")))

esa[which(esa$tentinh == "Long An"),] <- esa[which(esa$tentinh == "Long An"),] %>%
        mutate(tenxa = str_replace_all(tenxa, c("VÜnh Böu" = "Vĩnh Bửu",
                                                "VÜnh Công" = "Vĩnh Công",
                                                "Phó Ng·i TrÞ" = "Phu Ngai Tri",
                                                "Thánh Vinh Đông" = "Thanh Vĩnh Đông")))

x <- anti_join(panelid, esa, by = c("tentinh", "tenquan", "tenxa")) %>% 
        group_by(tentinh, tenquan, tenxa) %>% 
        distinct(tentinh, tenquan, tenxa)

z <-  esa %>% group_by(tentinh, tenquan, tenxa) %>% 
        filter(tentinh == "Điện Biên")

y <- esa %>% group_by(tentinh, tenquan, tenxa) %>% 
        filter(tenquan == "Thanh Uyên" & tentinh == "Lai Châu")

# Rename xa, huyen, tinh
nighttime <- nighttime %>% 
        rename(Tinh = NAME_1, Huyen = NAME_2, Xa = NAME_3) %>% 
        dplyr::select(-c(NAME_0, VARNAME_3, NL_NAME_3, CCA_3, CCN_3)) %>% 
        filter(year == 2013 | year == 2015 | year == 2017) %>% 
        mutate(year = case_when(year == 2013 ~ 2014,
                                year == 2015 ~ 2016,
                                year == 2017 ~ 2018))

nighttime$year <- as.numeric(nighttime$year)
