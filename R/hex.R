make_hex <- function() {
  imgurl <- system.file("fdp_img.png", package = "fdp")
  hexSticker::sticker(imgurl,
                      s_x = 1.06,
                      s_y = 1.0,
                      s_width = 1.1,
                      s_height = 1.1,
                      package = "",
                      p_x = 0.6,
                      p_y = 1.45,
                      p_color = "#FFFFFF",
                      p_family = "serif",
                      p_fontface = "bold",
                      p_size = 5.0,
                      h_fill = "#1F2528",
                      h_color = "#EDEDED",
                      url = "fdp.louisaslett.com",
                      u_size = 1.75,
                      u_family = "mono",
                      u_color = "#2F5E9C",
                      u_x = 1.78,
                      u_y = 0.6,
                      u_angle = 90.0,
                      white_around_sticker = TRUE,
                      filename = file.path("inst", "fdp_hex.png"),
                      dpi = 600L)
  usethis::use_logo(file.path("inst", "fdp_hex.png"), geometry = "480x556")
}
