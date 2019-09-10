module Lib where

import ClassyPrelude
import Codec.Xlsx
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (append, splitOn, strip, replace)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.CSV
import Text.HTML.Scalpel

type ItemId = Text

type StoreUrl = URL

type StoreName = Text

type Price = Text

type StoreUrlCtor = ItemId -> StoreUrl

type PriceScraper = Scraper Text Price

type StoreScraper = (StoreName, StoreUrlCtor, PriceScraper)

type StoreScrapingResult = (StoreName, [(ItemId, Price)])

type ItemsWithPrices = Map ItemId [(StoreName, Price)]

-- Utils
cutStart :: Text -> Text -> Text
cutStart whatToCut text =
  if whatToCut `isPrefixOf` text
    then drop (length whatToCut) text
    else text

cutEnd :: Text -> Text -> Text
cutEnd whatToCut text =
  if whatToCut `isSuffixOf` text
    then take (length text - length whatToCut) text
    else text

appendToEnd :: Text -> Text -> Text
appendToEnd whatToAppend text = text ++ whatToAppend

-- Excel parsing
extractCellText :: CellValue -> Text
extractCellText (CellText text) = text
extractCellText (CellDouble num) = tshow (round num :: Integer)
extractCellText _ = mempty

getFirstColumnValues :: Worksheet -> [CellValue]
getFirstColumnValues = mapMaybe _cellValue . toList . Map.filterWithKey (\(_, col) _ -> col == 1) . _wsCells

getFirstSheet :: Xlsx -> Maybe Worksheet
getFirstSheet = fmap snd . headMay . _xlSheets

parseIds :: L.ByteString -> [ItemId]
parseIds idsFile = do
  let idsSheet = getFirstSheet $ toXlsx idsFile
      ids = getFirstColumnValues <$> idsSheet
  case ids of
    Just ids -> nub . filter (/= mempty) . map extractCellText $ ids
    Nothing -> []

gaudiDsScraper :: StoreScraper
gaudiDsScraper = ("gaudi-ds", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id = "https://www.gaudi-ds.com/search/?orderby=position&orderway=desc&search_query=" <> unpack id
    scrapePrice = chroot ("span" @: [hasClass "price", hasClass "product-price"]) $ text anySelector

baniaBgScraper :: StoreScraper
baniaBgScraper = ("bania", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id = "https://shop.bania.bg/search.php?phrase=" <> unpack id
    scrapePrice = chroot ("span" @: [hasClass "price-value"]) $ text anySelector

formabaniaScraper :: StoreScraper
formabaniaScraper = ("formabania", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id = "https://formabania.bg/index.php?route=product/search&search=" <> unpack id <> "&description=true"
    scrapePrice =
      chroot ("span" @: [hasClass "price-tax"]) $ do
        priceWithTaxText <- text anySelector
        return $ cutStart "с ДДС: " priceWithTaxText

kristinBgScraper :: StoreScraper
kristinBgScraper = ("kristin.bg", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id =
      "https://www.kristin.bg/search.html?phrase=" <> unpack id <> "&action=dmExecAdvancedSearch&ProductSort=1"
    scrapePrice =
      chroot ("ul" @: [hasClass "grid", hasClass "product-page"]) $ text $ "span" @: [hasClass "price-value"]

baniamechtaScraper :: StoreScraper
baniamechtaScraper = ("baniamechta.com", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id = "https://baniamechta.com/?s=" <> unpack id <> "&post_type=product&fwp_sort=price_asc"
    scrapePrice = chroot ("span" @: [hasClass "woocommerce-Price-amount"]) $ text anySelector

bgmaistorScraper :: StoreScraper
bgmaistorScraper = ("bg-maistor.com", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id =
      "https://bg-maistor.com/component/virtuemart/search/" <> unpack id <> "/by,product_price/dirAsc?Itemid=0"
    scrapePrice = chroot ("span" @: [hasClass "PricesalesPrice"]) $ text anySelector

keranovaScraper :: StoreScraper
keranovaScraper = ("keranova.org", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id = "https://www.keranova.org/search?search=" <> unpack id
    scrapePrice = chroot ("span" @: [hasClass "new-price"]) $ text $ "span" @: [hasClass "total-price-value"]

marmagScraper :: StoreScraper
marmagScraper = ("marmag.bg", constructUrl, scrapePrice)
  where
    constructUrl :: StoreUrlCtor
    constructUrl id = "https://marmag.bg/catalogsearch/result/index/?dir=asc&order=price&q=" <> unpack id
    scrapePrice = chroot ("div" @: [hasClass "price-box"]) $ text $ "span" @: [hasClass "price"]

-- Scraping
scrapePrices :: [ItemId] -> StoreScraper -> IO StoreScrapingResult
scrapePrices ids (storeName, storeUrlCtor, scrapingStrategy) = do
  prices <- mapConcurrently scrapePrice ids
  return (storeName, prices)
  where
    scrapePrice id = do
      let storeUrl = storeUrlCtor id
      priceResult <- scrapeURL storeUrl scrapingStrategy `catch` (\(e :: SomeException) -> return Nothing)
      case priceResult of
        Just price -> do
          let price' = replace " " "" . strip $ price
          return (id, price')
        Nothing -> return (id, "not found")

toItemScrapeResults :: [StoreScrapingResult] -> ItemsWithPrices
toItemScrapeResults scrapingResults = do
  let allIds =
        concatMap
          (\(storeName, itemPrices) -> map (\(itemId, price) -> (itemId, (storeName, price))) itemPrices)
          scrapingResults
  sortAndGroup allIds
  where
    sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

pricesToCsv :: [StoreName] -> ItemsWithPrices -> CSV
pricesToCsv allStoreNames prices = do
  let header = ["Item Id" : map unpack (sort allStoreNames)] :: [Record]
      rows =
        Map.elems $
        Map.mapWithKey
          (\itemId storesWithPrices -> do
             let storePricesSorted = sortOn fst storesWithPrices
                 row = unpack itemId : map (unpack . snd) storePricesSorted :: [Field]
             row)
          prices :: [Record]
  header <> rows

scrapeStores :: FilePath -> IO ()
scrapeStores idsFilePath = do
  idsFile <- L.readFile idsFilePath
  let ids = parseIds idsFile
      scrapers =
        [ kristinBgScraper
        , baniamechtaScraper
        , bgmaistorScraper
        , keranovaScraper
        , marmagScraper
        , formabaniaScraper
        , baniaBgScraper
        , gaudiDsScraper
        ]
      allStoreNames = map (\(storeName, _, _) -> storeName) scrapers
  print "Starting to scrape aggressively..."
  itemsWithPrices <- toItemScrapeResults <$> mapConcurrently (scrapePrices ids) scrapers
  print "Finished!"
  let itemsCsv = L.fromStrict . B.pack $ printCSV $ pricesToCsv allStoreNames itemsWithPrices
  L.writeFile "exported.csv" itemsCsv