(ns fulcro.incubator.ui.icons.typicons
  (:refer-clojure :exclude [divide filter key map time])
  (:require ["react-icons/lib/ti" :as ti]
            [fulcro.client.dom :as dom]
            [fulcro.incubator.ui.core :as uc]))

(def adjust-brightness (uc/component-factory-simple ti/TiAdjustBrightness))
(def adjust-contrast (uc/component-factory-simple ti/TiAdjustContrast))
(def anchor (uc/component-factory-simple ti/TiAnchor))
(def anchor-outline (uc/component-factory-simple ti/TiAnchorOutline))
(def archive (uc/component-factory-simple ti/TiArchive))
(def arrow-back (uc/component-factory-simple ti/TiArrowBack))
(def arrow-back-outline (uc/component-factory-simple ti/TiArrowBackOutline))
(def arrow-down (uc/component-factory-simple ti/TiArrowDown))
(def arrow-down-outline (uc/component-factory-simple ti/TiArrowDownOutline))
(def arrow-down-thick (uc/component-factory-simple ti/TiArrowDownThick))
(def arrow-forward (uc/component-factory-simple ti/TiArrowForward))
(def arrow-forward-outline (uc/component-factory-simple ti/TiArrowForwardOutline))
(def arrow-left (uc/component-factory-simple ti/TiArrowLeft))
(def arrow-left-outline (uc/component-factory-simple ti/TiArrowLeftOutline))
(def arrow-left-thick (uc/component-factory-simple ti/TiArrowLeftThick))
(def arrow-loop (uc/component-factory-simple ti/TiArrowLoop))
(def arrow-loop-outline (uc/component-factory-simple ti/TiArrowLoopOutline))
(def arrow-maximise (uc/component-factory-simple ti/TiArrowMaximise))
(def arrow-maximise-outline (uc/component-factory-simple ti/TiArrowMaximiseOutline))
(def arrow-minimise (uc/component-factory-simple ti/TiArrowMinimise))
(def arrow-minimise-outline (uc/component-factory-simple ti/TiArrowMinimiseOutline))
(def arrow-move (uc/component-factory-simple ti/TiArrowMove))
(def arrow-move-outline (uc/component-factory-simple ti/TiArrowMoveOutline))
(def arrow-repeat (uc/component-factory-simple ti/TiArrowRepeat))
(def arrow-repeat-outline (uc/component-factory-simple ti/TiArrowRepeatOutline))
(def arrow-right (uc/component-factory-simple ti/TiArrowRight))
(def arrow-right-outline (uc/component-factory-simple ti/TiArrowRightOutline))
(def arrow-right-thick (uc/component-factory-simple ti/TiArrowRightThick))
(def arrow-shuffle (uc/component-factory-simple ti/TiArrowShuffle))
(def arrow-sorted-down (uc/component-factory-simple ti/TiArrowSortedDown))
(def arrow-sorted-up (uc/component-factory-simple ti/TiArrowSortedUp))
(def arrow-sync (uc/component-factory-simple ti/TiArrowSync))
(def arrow-sync-outline (uc/component-factory-simple ti/TiArrowSyncOutline))
(def arrow-unsorted (uc/component-factory-simple ti/TiArrowUnsorted))
(def arrow-up (uc/component-factory-simple ti/TiArrowUp))
(def arrow-up-outline (uc/component-factory-simple ti/TiArrowUpOutline))
(def arrow-up-thick (uc/component-factory-simple ti/TiArrowUpThick))
(def at (uc/component-factory-simple ti/TiAt))
(def attachment (uc/component-factory-simple ti/TiAttachment))
(def attachment-outline (uc/component-factory-simple ti/TiAttachmentOutline))
(def backspace (uc/component-factory-simple ti/TiBackspace))
(def backspace-outline (uc/component-factory-simple ti/TiBackspaceOutline))
(def battery-charge (uc/component-factory-simple ti/TiBatteryCharge))
(def battery-full (uc/component-factory-simple ti/TiBatteryFull))
(def battery-high (uc/component-factory-simple ti/TiBatteryHigh))
(def battery-low (uc/component-factory-simple ti/TiBatteryLow))
(def battery-mid (uc/component-factory-simple ti/TiBatteryMid))
(def beaker (uc/component-factory-simple ti/TiBeaker))
(def beer (uc/component-factory-simple ti/TiBeer))
(def bell (uc/component-factory-simple ti/TiBell))
(def book (uc/component-factory-simple ti/TiBook))
(def bookmark (uc/component-factory-simple ti/TiBookmark))
(def briefcase (uc/component-factory-simple ti/TiBriefcase))
(def brush (uc/component-factory-simple ti/TiBrush))
(def business-card (uc/component-factory-simple ti/TiBusinessCard))
(def calculator (uc/component-factory-simple ti/TiCalculator))
(def calendar (uc/component-factory-simple ti/TiCalendar))
(def calendar-outline (uc/component-factory-simple ti/TiCalendarOutline))
(def calender (uc/component-factory-simple ti/TiCalender))
(def calender-outline (uc/component-factory-simple ti/TiCalenderOutline))
(def camera (uc/component-factory-simple ti/TiCamera))
(def camera-outline (uc/component-factory-simple ti/TiCameraOutline))
(def cancel (uc/component-factory-simple ti/TiCancel))
(def cancel-outline (uc/component-factory-simple ti/TiCancelOutline))
(def chart-area (uc/component-factory-simple ti/TiChartArea))
(def chart-area-outline (uc/component-factory-simple ti/TiChartAreaOutline))
(def chart-bar (uc/component-factory-simple ti/TiChartBar))
(def chart-bar-outline (uc/component-factory-simple ti/TiChartBarOutline))
(def chart-line (uc/component-factory-simple ti/TiChartLine))
(def chart-line-outline (uc/component-factory-simple ti/TiChartLineOutline))
(def chart-pie (uc/component-factory-simple ti/TiChartPie))
(def chart-pie-outline (uc/component-factory-simple ti/TiChartPieOutline))
(def chevron-left (uc/component-factory-simple ti/TiChevronLeft))
(def chevron-left-outline (uc/component-factory-simple ti/TiChevronLeftOutline))
(def chevron-right (uc/component-factory-simple ti/TiChevronRight))
(def chevron-right-outline (uc/component-factory-simple ti/TiChevronRightOutline))
(def clipboard (uc/component-factory-simple ti/TiClipboard))
(def cloud-storage (uc/component-factory-simple ti/TiCloudStorage))
(def cloud-storage-outline (uc/component-factory-simple ti/TiCloudStorageOutline))
(def code (uc/component-factory-simple ti/TiCode))
(def code-outline (uc/component-factory-simple ti/TiCodeOutline))
(def coffee (uc/component-factory-simple ti/TiCoffee))
(def cog (uc/component-factory-simple ti/TiCog))
(def cog-outline (uc/component-factory-simple ti/TiCogOutline))
(def compass (uc/component-factory-simple ti/TiCompass))
(def contacts (uc/component-factory-simple ti/TiContacts))
(def credit-card (uc/component-factory-simple ti/TiCreditCard))
(def cross (uc/component-factory-simple ti/TiCross))
(def css3 (uc/component-factory-simple ti/TiCss3))
(def database (uc/component-factory-simple ti/TiDatabase))
(def delete (uc/component-factory-simple ti/TiDelete))
(def delete-outline (uc/component-factory-simple ti/TiDeleteOutline))
(def device-desktop (uc/component-factory-simple ti/TiDeviceDesktop))
(def device-laptop (uc/component-factory-simple ti/TiDeviceLaptop))
(def device-phone (uc/component-factory-simple ti/TiDevicePhone))
(def device-tablet (uc/component-factory-simple ti/TiDeviceTablet))
(def directions (uc/component-factory-simple ti/TiDirections))
(def divide (uc/component-factory-simple ti/TiDivide))
(def divide-outline (uc/component-factory-simple ti/TiDivideOutline))
(def document (uc/component-factory-simple ti/TiDocument))
(def document-add (uc/component-factory-simple ti/TiDocumentAdd))
(def document-delete (uc/component-factory-simple ti/TiDocumentDelete))
(def document-text (uc/component-factory-simple ti/TiDocumentText))
(def download (uc/component-factory-simple ti/TiDownload))
(def download-outline (uc/component-factory-simple ti/TiDownloadOutline))
(def dropbox (uc/component-factory-simple ti/TiDropbox))
(def edit (uc/component-factory-simple ti/TiEdit))
(def eject (uc/component-factory-simple ti/TiEject))
(def eject-outline (uc/component-factory-simple ti/TiEjectOutline))
(def equals (uc/component-factory-simple ti/TiEquals))
(def equals-outline (uc/component-factory-simple ti/TiEqualsOutline))
(def export (uc/component-factory-simple ti/TiExport))
(def export-outline (uc/component-factory-simple ti/TiExportOutline))
(def eye (uc/component-factory-simple ti/TiEye))
(def eye-outline (uc/component-factory-simple ti/TiEyeOutline))
(def feather (uc/component-factory-simple ti/TiFeather))
(def film (uc/component-factory-simple ti/TiFilm))
(def filter (uc/component-factory-simple ti/TiFilter))
(def flag (uc/component-factory-simple ti/TiFlag))
(def flag-outline (uc/component-factory-simple ti/TiFlagOutline))
(def flash (uc/component-factory-simple ti/TiFlash))
(def flash-outline (uc/component-factory-simple ti/TiFlashOutline))
(def flow-children (uc/component-factory-simple ti/TiFlowChildren))
(def flow-merge (uc/component-factory-simple ti/TiFlowMerge))
(def flow-parallel (uc/component-factory-simple ti/TiFlowParallel))
(def flow-switch (uc/component-factory-simple ti/TiFlowSwitch))
(def folder (uc/component-factory-simple ti/TiFolder))
(def folder-add (uc/component-factory-simple ti/TiFolderAdd))
(def folder-delete (uc/component-factory-simple ti/TiFolderDelete))
(def folder-open (uc/component-factory-simple ti/TiFolderOpen))
(def gift (uc/component-factory-simple ti/TiGift))
(def globe (uc/component-factory-simple ti/TiGlobe))
(def globe-outline (uc/component-factory-simple ti/TiGlobeOutline))
(def group (uc/component-factory-simple ti/TiGroup))
(def group-outline (uc/component-factory-simple ti/TiGroupOutline))
(def headphones (uc/component-factory-simple ti/TiHeadphones))
(def heart (uc/component-factory-simple ti/TiHeart))
(def heart-full-outline (uc/component-factory-simple ti/TiHeartFullOutline))
(def heart-half-outline (uc/component-factory-simple ti/TiHeartHalfOutline))
(def heart-outline (uc/component-factory-simple ti/TiHeartOutline))
(def home (uc/component-factory-simple ti/TiHome))
(def home-outline (uc/component-factory-simple ti/TiHomeOutline))
(def html5 (uc/component-factory-simple ti/TiHtml5))
(def image (uc/component-factory-simple ti/TiImage))
(def image-outline (uc/component-factory-simple ti/TiImageOutline))
(def index (uc/component-factory-simple ti/TiIndex))
(def infinity (uc/component-factory-simple ti/TiInfinity))
(def infinity-outline (uc/component-factory-simple ti/TiInfinityOutline))
(def info (uc/component-factory-simple ti/TiInfo))
(def info-large (uc/component-factory-simple ti/TiInfoLarge))
(def info-large-outline (uc/component-factory-simple ti/TiInfoLargeOutline))
(def info-outline (uc/component-factory-simple ti/TiInfoOutline))
(def input-checked (uc/component-factory-simple ti/TiInputChecked))
(def input-checked-outline (uc/component-factory-simple ti/TiInputCheckedOutline))
(def key (uc/component-factory-simple ti/TiKey))
(def key-outline (uc/component-factory-simple ti/TiKeyOutline))
(def keyboard (uc/component-factory-simple ti/TiKeyboard))
(def leaf (uc/component-factory-simple ti/TiLeaf))
(def lightbulb (uc/component-factory-simple ti/TiLightbulb))
(def link (uc/component-factory-simple ti/TiLink))
(def link-outline (uc/component-factory-simple ti/TiLinkOutline))
(def location (uc/component-factory-simple ti/TiLocation))
(def location-arrow (uc/component-factory-simple ti/TiLocationArrow))
(def location-arrow-outline (uc/component-factory-simple ti/TiLocationArrowOutline))
(def location-outline (uc/component-factory-simple ti/TiLocationOutline))
(def lock-closed (uc/component-factory-simple ti/TiLockClosed))
(def lock-closed-outline (uc/component-factory-simple ti/TiLockClosedOutline))
(def lock-open (uc/component-factory-simple ti/TiLockOpen))
(def lock-open-outline (uc/component-factory-simple ti/TiLockOpenOutline))
(def mail (uc/component-factory-simple ti/TiMail))
(def map (uc/component-factory-simple ti/TiMap))
(def media-eject (uc/component-factory-simple ti/TiMediaEject))
(def media-eject-outline (uc/component-factory-simple ti/TiMediaEjectOutline))
(def media-fast-forward (uc/component-factory-simple ti/TiMediaFastForward))
(def media-fast-forward-outline (uc/component-factory-simple ti/TiMediaFastForwardOutline))
(def media-pause (uc/component-factory-simple ti/TiMediaPause))
(def media-pause-outline (uc/component-factory-simple ti/TiMediaPauseOutline))
(def media-play (uc/component-factory-simple ti/TiMediaPlay))
(def media-play-outline (uc/component-factory-simple ti/TiMediaPlayOutline))
(def media-play-reverse (uc/component-factory-simple ti/TiMediaPlayReverse))
(def media-play-reverse-outline (uc/component-factory-simple ti/TiMediaPlayReverseOutline))
(def media-record (uc/component-factory-simple ti/TiMediaRecord))
(def media-record-outline (uc/component-factory-simple ti/TiMediaRecordOutline))
(def media-rewind (uc/component-factory-simple ti/TiMediaRewind))
(def media-rewind-outline (uc/component-factory-simple ti/TiMediaRewindOutline))
(def media-stop (uc/component-factory-simple ti/TiMediaStop))
(def media-stop-outline (uc/component-factory-simple ti/TiMediaStopOutline))
(def message (uc/component-factory-simple ti/TiMessage))
(def message-typing (uc/component-factory-simple ti/TiMessageTyping))
(def messages (uc/component-factory-simple ti/TiMessages))
(def microphone (uc/component-factory-simple ti/TiMicrophone))
(def microphone-outline (uc/component-factory-simple ti/TiMicrophoneOutline))
(def minus (uc/component-factory-simple ti/TiMinus))
(def minus-outline (uc/component-factory-simple ti/TiMinusOutline))
(def mortar-board (uc/component-factory-simple ti/TiMortarBoard))
(def news (uc/component-factory-simple ti/TiNews))
(def notes (uc/component-factory-simple ti/TiNotes))
(def notes-outline (uc/component-factory-simple ti/TiNotesOutline))
(def pen (uc/component-factory-simple ti/TiPen))
(def pencil (uc/component-factory-simple ti/TiPencil))
(def phone (uc/component-factory-simple ti/TiPhone))
(def phone-outline (uc/component-factory-simple ti/TiPhoneOutline))
(def pi (uc/component-factory-simple ti/TiPi))
(def pi-outline (uc/component-factory-simple ti/TiPiOutline))
(def pin (uc/component-factory-simple ti/TiPin))
(def pin-outline (uc/component-factory-simple ti/TiPinOutline))
(def pipette (uc/component-factory-simple ti/TiPipette))
(def plane (uc/component-factory-simple ti/TiPlane))
(def plane-outline (uc/component-factory-simple ti/TiPlaneOutline))
(def plug (uc/component-factory-simple ti/TiPlug))
(def plus (uc/component-factory-simple ti/TiPlus))
(def plus-outline (uc/component-factory-simple ti/TiPlusOutline))
(def point-of-interest (uc/component-factory-simple ti/TiPointOfInterest))
(def point-of-interest-outline (uc/component-factory-simple ti/TiPointOfInterestOutline))
(def power (uc/component-factory-simple ti/TiPower))
(def power-outline (uc/component-factory-simple ti/TiPowerOutline))
(def printer (uc/component-factory-simple ti/TiPrinter))
(def puzzle (uc/component-factory-simple ti/TiPuzzle))
(def puzzle-outline (uc/component-factory-simple ti/TiPuzzleOutline))
(def radar (uc/component-factory-simple ti/TiRadar))
(def radar-outline (uc/component-factory-simple ti/TiRadarOutline))
(def refresh (uc/component-factory-simple ti/TiRefresh))
(def refresh-outline (uc/component-factory-simple ti/TiRefreshOutline))
(def rss (uc/component-factory-simple ti/TiRss))
(def rss-outline (uc/component-factory-simple ti/TiRssOutline))
(def scissors (uc/component-factory-simple ti/TiScissors))
(def scissors-outline (uc/component-factory-simple ti/TiScissorsOutline))
(def shopping-bag (uc/component-factory-simple ti/TiShoppingBag))
(def shopping-cart (uc/component-factory-simple ti/TiShoppingCart))
(def social-at-circular (uc/component-factory-simple ti/TiSocialAtCircular))
(def social-dribbble (uc/component-factory-simple ti/TiSocialDribbble))
(def social-dribbble-circular (uc/component-factory-simple ti/TiSocialDribbbleCircular))
(def social-facebook (uc/component-factory-simple ti/TiSocialFacebook))
(def social-facebook-circular (uc/component-factory-simple ti/TiSocialFacebookCircular))
(def social-flickr (uc/component-factory-simple ti/TiSocialFlickr))
(def social-flickr-circular (uc/component-factory-simple ti/TiSocialFlickrCircular))
(def social-github (uc/component-factory-simple ti/TiSocialGithub))
(def social-github-circular (uc/component-factory-simple ti/TiSocialGithubCircular))
(def social-google-plus (uc/component-factory-simple ti/TiSocialGooglePlus))
(def social-google-plus-circular (uc/component-factory-simple ti/TiSocialGooglePlusCircular))
(def social-instagram (uc/component-factory-simple ti/TiSocialInstagram))
(def social-instagram-circular (uc/component-factory-simple ti/TiSocialInstagramCircular))
(def social-last-fm (uc/component-factory-simple ti/TiSocialLastFm))
(def social-last-fm-circular (uc/component-factory-simple ti/TiSocialLastFmCircular))
(def social-linkedin (uc/component-factory-simple ti/TiSocialLinkedin))
(def social-linkedin-circular (uc/component-factory-simple ti/TiSocialLinkedinCircular))
(def social-pinterest (uc/component-factory-simple ti/TiSocialPinterest))
(def social-pinterest-circular (uc/component-factory-simple ti/TiSocialPinterestCircular))
(def social-skype (uc/component-factory-simple ti/TiSocialSkype))
(def social-skype-outline (uc/component-factory-simple ti/TiSocialSkypeOutline))
(def social-tumbler (uc/component-factory-simple ti/TiSocialTumbler))
(def social-tumbler-circular (uc/component-factory-simple ti/TiSocialTumblerCircular))
(def social-twitter (uc/component-factory-simple ti/TiSocialTwitter))
(def social-twitter-circular (uc/component-factory-simple ti/TiSocialTwitterCircular))
(def social-vimeo (uc/component-factory-simple ti/TiSocialVimeo))
(def social-vimeo-circular (uc/component-factory-simple ti/TiSocialVimeoCircular))
(def social-youtube (uc/component-factory-simple ti/TiSocialYoutube))
(def social-youtube-circular (uc/component-factory-simple ti/TiSocialYoutubeCircular))
(def sort-alphabetically (uc/component-factory-simple ti/TiSortAlphabetically))
(def sort-alphabetically-outline (uc/component-factory-simple ti/TiSortAlphabeticallyOutline))
(def sort-numerically (uc/component-factory-simple ti/TiSortNumerically))
(def sort-numerically-outline (uc/component-factory-simple ti/TiSortNumericallyOutline))
(def spanner (uc/component-factory-simple ti/TiSpanner))
(def spanner-outline (uc/component-factory-simple ti/TiSpannerOutline))
(def spiral (uc/component-factory-simple ti/TiSpiral))
(def star (uc/component-factory-simple ti/TiStar))
(def star-full-outline (uc/component-factory-simple ti/TiStarFullOutline))
(def star-half (uc/component-factory-simple ti/TiStarHalf))
(def star-half-outline (uc/component-factory-simple ti/TiStarHalfOutline))
(def star-outline (uc/component-factory-simple ti/TiStarOutline))
(def starburst (uc/component-factory-simple ti/TiStarburst))
(def starburst-outline (uc/component-factory-simple ti/TiStarburstOutline))
(def stopwatch (uc/component-factory-simple ti/TiStopwatch))
(def support (uc/component-factory-simple ti/TiSupport))
(def tabs-outline (uc/component-factory-simple ti/TiTabsOutline))
(def tag (uc/component-factory-simple ti/TiTag))
(def tags (uc/component-factory-simple ti/TiTags))
(def th-large (uc/component-factory-simple ti/TiThLarge))
(def th-large-outline (uc/component-factory-simple ti/TiThLargeOutline))
(def th-list (uc/component-factory-simple ti/TiThList))
(def th-list-outline (uc/component-factory-simple ti/TiThListOutline))
(def th-menu (uc/component-factory-simple ti/TiThMenu))
(def th-menu-outline (uc/component-factory-simple ti/TiThMenuOutline))
(def th-small (uc/component-factory-simple ti/TiThSmall))
(def th-small-outline (uc/component-factory-simple ti/TiThSmallOutline))
(def thermometer (uc/component-factory-simple ti/TiThermometer))
(def thumbs-down (uc/component-factory-simple ti/TiThumbsDown))
(def thumbs-ok (uc/component-factory-simple ti/TiThumbsOk))
(def thumbs-up (uc/component-factory-simple ti/TiThumbsUp))
(def tick (uc/component-factory-simple ti/TiTick))
(def tick-outline (uc/component-factory-simple ti/TiTickOutline))
(def ticket (uc/component-factory-simple ti/TiTicket))
(def time (uc/component-factory-simple ti/TiTime))
(def times (uc/component-factory-simple ti/TiTimes))
(def times-outline (uc/component-factory-simple ti/TiTimesOutline))
(def trash (uc/component-factory-simple ti/TiTrash))
(def tree (uc/component-factory-simple ti/TiTree))
(def upload (uc/component-factory-simple ti/TiUpload))
(def upload-outline (uc/component-factory-simple ti/TiUploadOutline))
(def user (uc/component-factory-simple ti/TiUser))
(def user-add (uc/component-factory-simple ti/TiUserAdd))
(def user-add-outline (uc/component-factory-simple ti/TiUserAddOutline))
(def user-delete (uc/component-factory-simple ti/TiUserDelete))
(def user-delete-outline (uc/component-factory-simple ti/TiUserDeleteOutline))
(def user-outline (uc/component-factory-simple ti/TiUserOutline))
(def vendor-android (uc/component-factory-simple ti/TiVendorAndroid))
(def vendor-apple (uc/component-factory-simple ti/TiVendorApple))
(def vendor-microsoft (uc/component-factory-simple ti/TiVendorMicrosoft))
(def video (uc/component-factory-simple ti/TiVideo))
(def video-outline (uc/component-factory-simple ti/TiVideoOutline))
(def volume (uc/component-factory-simple ti/TiVolume))
(def volume-down (uc/component-factory-simple ti/TiVolumeDown))
(def volume-mute (uc/component-factory-simple ti/TiVolumeMute))
(def volume-up (uc/component-factory-simple ti/TiVolumeUp))
(def warning (uc/component-factory-simple ti/TiWarning))
(def warning-outline (uc/component-factory-simple ti/TiWarningOutline))
(def watch (uc/component-factory-simple ti/TiWatch))
(def waves (uc/component-factory-simple ti/TiWaves))
(def waves-outline (uc/component-factory-simple ti/TiWavesOutline))
(def weather-cloudy (uc/component-factory-simple ti/TiWeatherCloudy))
(def weather-downpour (uc/component-factory-simple ti/TiWeatherDownpour))
(def weather-night (uc/component-factory-simple ti/TiWeatherNight))
(def weather-partly-sunny (uc/component-factory-simple ti/TiWeatherPartlySunny))
(def weather-shower (uc/component-factory-simple ti/TiWeatherShower))
(def weather-snow (uc/component-factory-simple ti/TiWeatherSnow))
(def weather-stormy (uc/component-factory-simple ti/TiWeatherStormy))
(def weather-sunny (uc/component-factory-simple ti/TiWeatherSunny))
(def weather-windy (uc/component-factory-simple ti/TiWeatherWindy))
(def weather-windy-cloudy (uc/component-factory-simple ti/TiWeatherWindyCloudy))
(def wi-fi (uc/component-factory-simple ti/TiWiFi))
(def wi-fi-outline (uc/component-factory-simple ti/TiWiFiOutline))
(def wine (uc/component-factory-simple ti/TiWine))
(def world (uc/component-factory-simple ti/TiWorld))
(def world-outline (uc/component-factory-simple ti/TiWorldOutline))
(def zoom (uc/component-factory-simple ti/TiZoom))
(def zoom-in (uc/component-factory-simple ti/TiZoomIn))
(def zoom-in-outline (uc/component-factory-simple ti/TiZoomInOutline))
(def zoom-out (uc/component-factory-simple ti/TiZoomOut))
(def zoom-out-outline (uc/component-factory-simple ti/TiZoomOutOutline))
(def zoom-outline (uc/component-factory-simple ti/TiZoomOutline))
