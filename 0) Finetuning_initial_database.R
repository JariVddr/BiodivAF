data <- data %>%
  mutate(Spec_group = Spec, Overall = "X", Crop_group = Crop.spec)

########################## Species groups ######################################

data$Soil_fauna[data$Spec == "Bacteria"] <- "X"
data$Spec_group[data$Spec == "Bacteria"] <- "Microbiota"

data$Natural_enemies[data$Spec == "Bats"] <- "X"
data$Flying_Vertebrates[data$Spec == "Bats"] <- "X"
data$Spec_group[data$Spec == "Bats"] <- "Chiroptera"

data$Spec[data$Spec == "Coccinelids"] <- "Ladybugs"
data$Spec[data$Spec == "Coccinellids"] <- "Ladybugs"
data$Spec[data$Spec == "Ladybirds"] <- "Ladybugs"
data$Pest_species[data$Spec == "Curculionids"] <- "X"
data$Pest_species[data$Spec == "Chrysomelids"] <- "X"
data$Pest_species[data$Spec == "Elator beetles"] <- "X"
data$Soil_fauna[data$Spec == "Carabids"] <- "X"
data$Soil_fauna[data$Spec == "Staphylinids"] <- "X"
data$Soil_fauna[data$Spec == "Dung beetles"] <- "X"
data$Natural_enemies[data$Spec == "Carabids"] <- "X"
data$Natural_enemies[data$Spec == "Ladybugs"] <- "X"
data$Natural_enemies[data$Spec == "Staphylinids"] <- "X"
data$Natural_enemies[data$Spec == "Beetles"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Beetles"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Curculionids"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Chrysomelids"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Ladybugs"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Elator beetles"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Carabids"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Staphylinids"] <- "X"
data$Spec_group[data$Spec == "Beetles"] <- "Coleoptera"
data$Spec_group[data$Spec == "Carabids"] <- "Coleoptera"
data$Spec_group[data$Spec == "Ladybugs"] <- "Coleoptera"
data$Spec_group[data$Spec == "Curculionids"] <- "Coleoptera"
data$Spec_group[data$Spec == "Chrysomelids"] <- "Coleoptera"
data$Spec_group[data$Spec == "Staphilinids"] <- "Coleoptera"
data$Spec_group[data$Spec == "Dung beetles"] <- "Coleoptera"
data$Spec_group[data$Spec == "Elator beetles"] <- "Coleoptera"

data$Natural_enemies[data$Spec == "Birds"] <- "X"
data$Flying_Vertebrates[data$Spec == "Birds"] <- "X"
data$Spec_group[data$Spec == "Birds"] <- "Aves"

data$Spec[data$Spec == "Blattaria"] <- "Cockroaches"
data$Pest_species[data$Spec == "Cockroaches"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Cockroaches"] <- "X"
data$Spec_group[data$Spec == "Cockroaches"] <- "Blattaria"

data$Spec[data$Spec == "Blattaria"] <- "Cockroaches"
data$Pest_species[data$Spec == "Cockroaches"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Cockroaches"] <- "X"
data$Spec_group[data$Spec == "Cockroaches"] <- "Blattaria"

data$Spec[data$Spec == "Hemiptera"] <- "Bugs"
data$Spec[data$Spec == "Homoptera"] <- "Bugs"
data$Pest_species[data$Spec == "Bugs"] <- "X"
data$Pest_species[data$Spec == "Aphids"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Aphids"] <- "X"
data$Aboveground_Arthropods[data$Spec == "bugs"] <- "X"
data$Spec_group[data$Spec == "Aphids"] <- "Hemiptera"
data$Spec_group[data$Spec == "Bugs"] <- "Hemiptera"

data$Spec[data$Spec == "Lepidoptera - caterpillar"] <- "Caterpillars"
data$Spec[data$Spec == "Lepidoptera - egg"] <- "Caterpillars"
data$Pollinators[data$Spec == "Butterflies"] <- "X"
data$Pollinators[data$Spec == "Moths"] <- "X"
data$Pollinators[data$Spec == "Lepidoptera"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Butterflies"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Moths"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Lepidoptera"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Caterpillars"] <- "X"
data$Spec_group[data$Spec == "Butterflies"] <- "Lepidoptera"
data$Spec_group[data$Spec == "Moths"] <- "Lepidoptera"
data$Spec_group[data$Spec == "Caterpillars"] <- "Lepidoptera"

data$Aboveground_Arthropods[data$Spec == "Caddisflies"] <- "X"
data$Spec_group[data$Spec == "Caddisflies"] <- "Trichoptera"

data$Spec[data$Spec == "Chilopoda"] <- "Centipedes"
data$Spec[data$Spec == "Geophilomorpha"] <- "Centipedes"
data$Spec[data$Spec == "Lithobiomorpha"] <- "Centipedes"
data$Soil_fauna[data$Spec == "Centipedes"] <- "X"
data$Natural_enemies[data$Spec == "Centipedes"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Centipedes"] <- "X"
data$Spec_group[data$Spec == "Centipedes"] <- "Chilopoda"

data$Spec[data$Spec == "Collembola"] <- "Springtails"
data$Soil_fauna[data$Spec == "Springtails"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Springtails"] <- "X"
data$Spec_group[data$Spec == "Springtails"] <- "Collembola"

data$Spec[data$Spec == "Decapodes"] <- "Decapods"
data$Soil_fauna[data$Spec == "Decapods"] <- "X"
data$Natural_enemies[data$Spec == "Decapods"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Decapods"] <- "X"
data$Spec_group[data$Spec == "Decapods"] <- "Decapoda"

data$Spec[data$Spec == "Dermaptera"] <- "Earwigs"
data$Soil_fauna[data$Spec == "Earwigs"] <- "X"
data$Natural_enemies[data$Spec == "Earwigs"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Earwigs"] <- "X"
data$Spec_group[data$Spec == "Earwigs"] <- "Dermaptera"

data$Soil_fauna[data$Spec == "Detritivores"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Detritivores"] <- "X"
data$Soil_fauna[data$Spec == "Soil fauna"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Soil fauna"] <- "X"

data$Spec[data$Spec == "Diptera"] <- "Flies"
data$Spec[data$Spec == "Syrphids"] <- "Hoverflies"
data$Spec[data$Spec == "Syrphidae"] <- "Hoverflies"
data$Pollinators[data$Spec == "Flies"] <- "X"
data$Pollinators[data$Spec == "Hoverflies"] <- "X"
data$Natural_enemies[data$Spec == "Slender-legged flies"] <- "X"
data$Natural_enemies[data$Spec == "Robber flies"] <- "X"
data$Pest_species[data$Spec == "Flies"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Flies"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Hoverflies"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Slender-legged flies"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Robber flies"] <- "X"
data$Spec_group[data$Spec == "Flies"] <- "Diptera"
data$Spec_group[data$Spec == "Hoverflies"] <- "Diptera"
data$Spec_group[data$Spec == "Slender-legged flies"] <- "Diptera"
data$Spec_group[data$Spec == "Robber flies"] <- "Diptera"

data$Spec[data$Spec == "Odonata"] <- "Dragonflies"
data$Aboveground_Arthropods[data$Spec == "Dragonflies"] <- "X"
data$Natural_enemies[data$Spec == "Dragonflies"] <- "X"
data$Spec_group[data$Spec == "Dragonflies"] <- "Odonata"

data$Spec[data$Spec == "Lumbricidae"] <- "Earthworms"
data$Soil_fauna[data$Spec == "Earthworms"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Earthworms"] <- "X"
data$Spec_group[data$Spec == "Earthworms"] <- "Lumbricidae"

data$Spec[data$Spec == "Ephemeroptera"] <- "Mayflies"
data$Aboveground_Arthropods[data$Spec == "Mayflies"] <- "X"
data$Spec_group[data$Spec == "Mayflies"] <- "Ephemeroptera"

data$Soil_fauna[data$Spec == "Fungi"] <- "X"
data$Spec_group[data$Spec == "Fungi"] <- "Microbiota"

data$Spec[data$Spec == "Gastropoda"] <- "Gastropods"
data$Spec[data$Spec == "Snail"] <- "Gastropods"
data$Pest_species[data$Spec == "Gastropods"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Gastropods"] <- "X"
data$Spec_group[data$Spec == "Gastropods"] <- "Gastropoda"

data$Spec[data$Spec == "Orthoptera"] <- "Grasshoppers"
data$Pest_species[data$Spec == "Grasshoppers"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Grasshoppers"] <- "X"
data$Spec_group[data$Spec == "Grasshoppers"] <- "Orthoptera"

data$Spec[data$Spec == "Opiliones"] <- "Harvestmen"
data$Natural_enemies[data$Spec == "Harvestmen"] <- "X"
data$Soil_fauna[data$Spec == "Harvestmen"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Harvestmen"] <- "X"
data$Spec_group[data$Spec == "Harvestmen"] <- "Opiliones"

data$Pest_species[data$Spec == "Herbivores"] <- "X"
data$Pest_species[data$Spec == "Pest species"] <- "X"
data$Spec_group[data$Spec == "Herbivores"] <- "Arthropods"
data$Spec_group[data$Spec == "Pest species"] <- "Arthropods"

data$Spec[data$Spec == "Planys"] <- "Plants"
data$Spec[data$Spec == "Weeds"] <- "Herbs"
data$Autotrophs[data$Spec == "Plants"] <- "X"
data$Autotrophs[data$Spec == "Herbs"] <- "X"
data$Autotrophs[data$Spec == "Shrubs"] <- "X"
data$Autotrophs[data$Spec == "Trees"] <- "X"
data$Pest_species[data$Spec == "Plants"] <- "X"
data$Pest_species[data$Spec == "Herbs"] <- "X"
data$Spec_group[data$Spec == "Plants"] <- "Plantae"
data$Spec_group[data$Spec == "Herbs"] <- "Plantae"
data$Spec_group[data$Spec == "Shrubs"] <- "Plantae"
data$Spec_group[data$Spec == "Trees"] <- "Plantae"

data$Natural_enemies[data$Spec == "Herpetofauna"] <- "X"
data$Ground_dwelling_Vertebrates[data$Spec == "Herpetofauna"] <- "X"
data$Pest_species[data$Spec == "Small mammals"] <- "X"
data$Ground_dwelling_Vertebrates[data$Spec == "Small mammals"] <- "X"

data$Spec[data$Spec == "Hirudinea"] <- "Leeches"
data$Soil_fauna[data$Spec == "Leeches"] <- "X"
data$Natural_enemies[data$Spec == "Leeches"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Leeches"] <- "X"
data$Spec_group[data$Spec == "Leeches"] <- "Hirudinea"

data$Soil_fauna[data$Spec == "Ants"] <- "X"
data$Natural_enemies[data$Spec == "Ants"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Ants"] <- "X"
data$Spec_group[data$Spec == "Ants"] <- "Hymenoptera"
data$Spec[data$Spec == "Bumblebees"] <- "Bees"
data$Pollinators[data$Spec == "Bees"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Bees"] <- "X"
data$Spec_group[data$Spec == "Ants"] <- "Hymenoptera"
data$Pollinators[data$Spec == "wasps"] <- "X"
data$Natural_enemies[data$Spec == "wasps"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Wasps"] <- "X"
data$Spec_group[data$Spec == "Wasps"] <- "Hymenoptera"
data$Pollinators[data$Spec == "Hymenoptera"] <- "X"
data$Natural_enemies[data$Spec == "Hymenoptera"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Hymenoptera"] <- "X"
data$Spec_group[data$Spec == "Hymenoptera"] <- "Hymenoptera"

data$Spec[data$Spec == "Isopods"] <- "Woodlice"
data$Soil_fauna[data$Spec == "Woodlice"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Woodlice"] <- "X"
data$Spec_group[data$Spec == "Woodlice"] <- "Isopoda"

data$Spec[data$Spec == "Neuroptera"] <- "Lacewings"
data$Aboveground_Arthropods[data$Spec == "Lacewings"] <- "X"
data$Natural_enemies[data$Spec == "Lacewings"] <- "X"
data$Spec_group[data$Spec == "Lacewings"] <- "Neuroptera"

data$Spec[data$Spec == "Thysanoptera"] <- "Thrips"
data$Aboveground_Arthropods[data$Spec == "Thrips"] <- "X"
data$Pest_species[data$Spec == "Thrips"] <- "X"
data$Spec_group[data$Spec == "Thrips"] <- "Thysanoptera"

data$Spec[data$Spec == "Silphonaptera"] <- "Fleas"
data$Pest_species[data$Spec == "Fleas"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Fleas"] <- "X"
data$Spec_group[data$Spec == "Fleas"] <- "Silphonaptera"

data$Autotrophs[data$Spec == "Lichen"] <- "X"

data$Natural_enemies[data$Spec == "Megaloptera"] <- "X"
data$Aboveground_Arthropods[data$Spec == "Megaloptera"] <- "X"

data$Soil_fauna[data$Spec == "Millipedes"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Millipedes"] <- "X"
data$Spec_group[data$Spec == "Millipedes"] <- "Diplopoda"

data$Soil_fauna[data$Spec == "Mites"] <- "X"
data$Natural_enemies[data$Spec == "Mites"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Mites"] <- "X"
data$Spec_group[data$Spec == "Mites"] <- "Acari"

data$Spec[data$Spec == "Bryophytes"] <- "Mosses"
data$Spec[data$Spec == "Moss"] <- "Mosses"
data$Autotrophs[data$Spec == "Mosses"] <- "X"
data$Spec_group[data$Spec == "Mosses"] <- "Bryophyta"

data$Soil_fauna[data$Spec == "Nematodes"] <- "X"
data$Pest_species[data$Spec == "Nematodes"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Nematodes"] <- "X"
data$Spec_group[data$Spec == "Nematodes"] <- "Nematoda"

data$Natural_enemies[data$Spec == "Natural enemies"] <- "X"
data$Natural_enemies[data$Spec == "Natural enemies - Arthropods"] <- "X"
data$Natural_enemies[data$Spec == "Parasites"] <- "X"
data$Natural_enemies[data$Spec == "Parasitoids"] <- "X"
data$Natural_enemies[data$Spec == "Predators"] <- "X"
data$Spec_group[data$Spec == "Natural enemies - Arthropods"] <- "Arthropods"
data$Spec_group[data$Spec == "Parasites"] <- "Arthropods"
data$Spec_group[data$Spec == "Parasitoids"] <- "Arthropods"

data$Pollinators[data$Spec == "Pollinators"] <- "X"
data$Spec_group[data$Spec == "Pollinators"] <- "Arthropods"

data$Spec[data$Spec == "Pseudoscorpiones"] <- "Pseudoscorpions"
data$Soil_fauna[data$Spec == "Pseudoscorpions"] <- "X"
data$Natural_enemies[data$Spec == "Pseudoscorpions"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Pseudoscorpions"] <- "X"
data$Spec_group[data$Spec == "Pseudoscorpions"] <- "Pseudoscorpiones"

data$Spec[data$Spec == "Araneae"] <- "Spiders"
data$Natural_enemies[data$Spec == "Spiders"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Spiders"] <- "X"
data$Spec_group[data$Spec == "Spiders"] <- "Araneae"

data$Spec[data$Spec == "Plecoptera"] <- "Stoneflies"
data$Aboveground_Arthropods[data$Spec == "Stoneflies"] <- "X"
data$Spec_group[data$Spec == "Stoneflies"] <- "Plecoptera"

data$Spec[data$Spec == "Psocoptera"] <- "Barkflies"
data$Pest_species[data$Spec == "Barkflies"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Barkflies"] <- "X"
data$Spec_group[data$Spec == "Barkflies"] <- "Psocoptera"

data$Soil_fauna[data$Spec == "Silverfishes"] <- "X"
data$Soil_related_Arthropods[data$Spec == "Silverfishes"] <- "X"
data$Spec_group[data$Spec == "Silverfishes"] <- "Zygentoma"

data$Arthropods[data$Spec_group == "Arthropods"] <- "X"
data$Arthropods[data$Soil_related_Arthropods == "X"] <- "X"
data$Arthropods[data$Vegetation_dwelling_Arthropods == "X"] <- "X"
data$Arthropods[data$Flying_Arthropods == "Arthropods"] <- "X"

######################### Metric #######################################

# Total SR homogenization (n == 1, Metric == SR)
data$Metric[data$Metric == "SR" & data$n == 1] <- "totSR"

######################### Age #######################################

# Age homogenization (Mature == 60)
data$Age[data$Age == "Mature"] <- "60" 
data$Age <- as.numeric(data$Age)
# Age capping on 60 years
data$Age[data$Age > 60] <- 60 

######################### System types #######################################

# Homogenization of System
data$System[data$System == "Forest grazing"] <- "Silvopasture"  # Forest grazing is in strictu senso often a very scarcely grazed silvopastoral system
data$System[data$System == "Silvopastoral"] <- "Silvopasture" 
data$System[data$System == "Silvopastoral"] <- "Silvopasture" 
data$Specif[data$System == "Forest"] <- "Native forest"
data$Specif[data$System == "Forest plantation"] <- "Plantation"
data$System[data$System == "Forest plantation"] <- "Forest"

######################### Crops #######################################

# Homogenization of crop types
data$Crop_group[data$Crop.spec == "Barley"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Wheat"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Maize"] <- "Corn"
data$Crop_group[data$Crop.spec == "Alfalfa"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Brassica"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Buckwheat"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Beetroot, Wheat, Beans, Rapeseed"] <- NA
data$Crop_group[data$Crop.spec == "Barley, Peas"] <- NA
data$Crop_group[data$Crop.spec == "Barley, Peas, Vetch"] <- NA
data$Crop_group[data$Crop.spec == "Cabbage"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Clover"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Crop rotation"] <- NA
data$Crop_group[data$Crop.spec == "Grass"] <- "Grass"
data$Crop_group[data$Crop.spec == "Brome"] <- "Grass"
data$Crop_group[data$Crop.spec == "Lettuce"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Zucchini"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Ley"] <- "Ley"
data$Crop_group[data$Crop.spec == "Maize, Barley, Wheat, Oilseed"] <- NA
data$Crop_group[data$Crop.spec == "Oats"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Millet"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Oilseed"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Strawberries"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Peas"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Beans"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Carrots"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Rapeseed"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Rapeseed, Wheat, Barley"] <- NA
data$Crop_group[data$Crop.spec == "Wheat, Grass"] <- NA
data$Crop_group[data$Crop.spec == "Rapeseed, Wheat, Peas, Barley, Sorghum"] <- NA
data$Crop_group[data$Crop.spec == "Rye"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Soybean"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Sunflower"] <- "Mass-flowering crops"
data$Crop_group[data$Crop.spec == "Sunflower, Wheat, Barley"] <- NA
data$Crop_group[data$Crop.spec == "Tomato"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Tomatoes"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Vetch"] <- "Vegetables"
data$Crop_group[data$Crop.spec == "Vineyard"] <- "Orchard"
data$Crop_group[data$Crop.spec == "Wheat"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Wheat"] <- "Cereals"
data$Crop_group[data$Crop.spec == "Wheat, Oilseed"] <- NA
data$Crop_group[data$Crop.spec == "Wheat, Rapeseed"] <- NA
data$Crop_group[data$Crop.spec == "Wheat, Rapeseed, Peas"] <- NA
data$Crop_group[data$Crop.spec == "Wheat, Rapeseed, Sunflower"] <- NA
data$Crop_group[data$Crop.spec == "Wheat/Barley"] <- "Cereals"

# Further division
data$Crop_group[data$Crop.spec == "Ley"] <- "Grass/Ley"
data$Crop_group[data$Crop.spec == "Grass"] <- "Grass/Ley"
data$Crop_group[data$Crop.spec == "Vegetables"] <- "Vegetables and mass-flowering crops"
data$Crop_group[data$Crop.spec == "Mass-flowering crop"] <- "Vegetables and mass-flowering crops"

######################### Org_Conv #######################################

# Type mistake conversion
data$Org.Conv[data$Org.Conv == " Org"] <- "Org"
data$Org.Conv[data$Org.Conv == " Conv"] <- "Conv"
data$Org.Conv[data$Org.Conv == "  Org"] <- "Org"
data$Org.Conv[data$Org.Conv == "  Conv"] <- "Conv"
data$Org.Conv[data$Org.Conv == "Org "] <- "Org"
data$Org.Conv[data$Org.Conv == "Conv "] <- "Conv"
data$Org.Conv[data$Org.Conv == " Org "] <- "Org"
data$Org.Conv[data$Org.Conv == " Conv "] <- "Conv"
data$Org.Conv[data$Org.Conv == "Org "] <- "Org"
data$Org.Conv[data$Org.Conv == "Conv "] <- "Conv"
data$Org.Conv[data$Org.Conv == "org"] <- "Org"
data$Org.Conv[data$Org.Conv == "conv"] <- "Conv"

######################### Climate #######################################

# Type mistake conversion
data$Climate[data$Climate == "N"] <- "T"
# Make more clear
data$Climate[data$Climate == "T"] <- "Temperate"
data$Climate[data$Climate == "M"] <- "Mediterranean"
data$Climate[data$Climate == "B"] <- "Boreal/Alpine"

######################### Woody features #######################################
data <- data %>%
  mutate(ID = row_number()) %>%
  mutate(Tree.spec = ifelse(Tree.spec == "" | is.na(Tree.spec), NA_character_, Tree.spec)) %>%
  mutate(Tree.spec = str_replace_all(Tree.spec, ", ", ",")) %>%  # Standardize separator to ", "
  mutate(Tree.spec = str_replace_all(Tree.spec, " and ", ",")) %>%  # Standardize separator to ", "
  separate(Tree.spec, into = paste0("Tree", 1:20), sep = ",", fill = "right", remove = FALSE) %>%  
  mutate(across(starts_with("Tree"), ~ifelse(is.na(Tree.spec), NA, .))) %>%  # Ensure all Tree columns are NA if Tree.spec is NA
  dplyr::select(-Tree.spec) %>%
  pivot_longer(cols = starts_with("Tree") & !matches("Tree.height") & !matches("Tree.diameter"),  
               names_to = "Tree", values_to = "Tree_spec") %>%  
  dplyr::select(-Tree) %>%
  distinct() %>% # Remove duplicates by NA value creation in longformat
  mutate(PA = 0) %>% mutate(PA = ifelse(!is.na(Tree_spec), 1, PA)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Salix ssp", "Salix", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "corylus", "Corylus", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Coryllus", "Corylus", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Larch", "Larix", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "others", "mixed", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Mixed", "mixed", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Mixede", "mixed", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Alnus glutinosa", "Alnus", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Pinus sylvestris", "Pinus", Tree_spec)) %>%
  mutate(Tree_spec = ifelse(Tree_spec == "Betula pubescens", "Betula", Tree_spec)) %>%
  # Step 1: Count the number of unique species (Tree_spec) for each observation (row)
  group_by(ID) %>%
  mutate(Tree_SR = n_distinct(na.omit(Tree_spec))) %>%  # Count unique species within each group
  ungroup() %>% 
  # Step 2: For SR == 1, select the species that are present in Tree_spec
  mutate(Tree.spec = NA_character_) %>%
  mutate(Tree.spec = ifelse(Tree_SR == 1 , Tree_spec, NA_character_)) %>%
  filter(!(Tree_SR == 1 & is.na(Tree_spec)))

# Small step inbetween to update species when needed
data %>% count(Tree_spec) %>% arrange(Tree_spec)

# Pivoting for other purposes
data <- data %>% mutate(PA = as.numeric(PA)) %>%
  pivot_wider(names_from = Tree_spec, values_from = PA, values_fill = list(PA = 0)) %>%
  dplyr::select(-'NA') %>%
  # Step 3: For Mixed == 1, assign Tree_SR = max(Tree_SR) within that group
  mutate(Tree_SR = ifelse(mixed == 1, max(Tree_SR, na.rm = TRUE), Tree_SR)) %>%
  mutate(Tree_SR = ifelse(Tree_SR == 0, NA, Tree_SR)) %>%
  # Step 4: Create a Tree_layer vector (TRUE or FALSE) based on species in the tree layer
  mutate(Shrub_layer = FALSE, Tree_layer = FALSE, Tree.pol = FALSE) %>%
  mutate(Tree_layer = ifelse(Abies == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Acer == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Alnus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Betula == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Caragana == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Carpinus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Carya == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Castanea == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Celtis == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Cistus == 1, TRUE, Shrub_layer)) %>%
  mutate(Shrub_layer = ifelse(Cornus == 1, TRUE, Shrub_layer)) %>%
  mutate(Shrub_layer = ifelse(Corylus == 1, TRUE, Shrub_layer)) %>%
  mutate(Shrub_layer = ifelse(Crataegus == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Cupressus == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Cytisus == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Eucalyptus == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Euonymus == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Fagus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Fraxinus == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Genista == 1, TRUE, Shrub_layer)) %>%
  mutate(Shrub_layer = ifelse(Ilex == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Juglans == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Larix == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Malus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Morus == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Nothofagus == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Olea == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Ostrya == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Paulownia == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Picea == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Pinus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Platanus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Populus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Prunus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Pseudotsuga == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Pyrus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Quercus == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Retama == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Robinia == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Rosa == 1, TRUE, Shrub_layer)) %>%
  mutate(Shrub_layer = ifelse(Rubus == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Salix == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Sambucus == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Sorbus == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Thuja == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Tilia == 1, TRUE, Tree_layer)) %>%
  mutate(Tree_layer = ifelse(Ulmus == 1, TRUE, Tree_layer)) %>%
  mutate(Shrub_layer = ifelse(Viburnum == 1, TRUE, Shrub_layer)) %>%
  mutate(Shrub_layer = ifelse(Vitis == 1, TRUE, Shrub_layer)) %>%
  mutate(Tree_layer = ifelse(Zelkova == 1, TRUE, Tree_layer)) %>%
  # Step 4.b: Group woody species to pollinating typologies
  mutate(Tree.pol = ifelse(Caragana == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Carya == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Celtis == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Cistus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Cornus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Crataegus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Cytisus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Eucalyptus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Euonymus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Genista == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Ilex == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Malus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Morus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Olea == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Paulownia == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Populus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Prunus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Pyrus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Retama == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Robinia == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Rosa == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Rubus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Salix == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Sambucus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Sorbus == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Tilia == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Viburnum == 1, TRUE, Tree.pol)) %>%
  mutate(Tree.pol = ifelse(Vitis == 1, TRUE, Tree.pol)) %>%
  # Step 5: Group woody species to typologies
  mutate(Tree.cat = NA_character_) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Abies", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Acer", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Alnus", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Betula", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Caragana", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Carpinus", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Carya", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Castanea", "Nut-producers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Celtis", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Cistus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Cornus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Corylus", "Nut-producers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Crataegus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Cupressus", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Cytisus", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Eucalyptus", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Euonymus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Fagus", "Nut-producers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Fraxinus", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Genista", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Ilex", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Juglans", "Nut-producers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Larix", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Malus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Morus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Nothofagus", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Olea", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Ostrya", "BOther broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Paulownia", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Picea", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Pinus", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Platanus", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Populus", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Prunus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Pseudotsuga", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Pyrus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Quercus", "Nut-producers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Retama", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Robinia", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Rosa", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Rubus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Salix", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Sambucus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Sorbus", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Thuja", "Conifers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Tilia", "Flower-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Ulmus", "Other broadleaveds", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Viburnum", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Vitis", "Fruit-bearers", Tree.cat)) %>%
  mutate(Tree.cat = ifelse(Tree.spec == "Zelkova", "Other broadleaveds", Tree.cat))

# Age capping on 60 years
data$Tree_SR[data$Tree_SR > 5] <- 5

######################### Tree dbh #######################################

# Unit homogenization (m to cm)
data$Tree.diameter <- as.numeric(data$Tree.diameter)
data <- data %>%
  mutate(Tree.diameter = ifelse(Tree.diameter < 3, Tree.diameter * 100, Tree.diameter))

######################### Loc_Region #######################################

library(geosphere)  # For distance calculations
library(sf)       # For spatial operations

# Extract coordinates from Loc_region
data <- data %>% 
  # Extract lat/lng from Loc_region
  separate(Loc_region, into = c("lat_region", "lng_region"), sep = ", ", extra = "merge", remove = FALSE) %>%
  separate(lat_region, into = c("lat_region", "lng_region2"), sep = ",", extra = "merge", remove = TRUE) %>%
  mutate(
    lng_region = ifelse(is.na(lng_region), lng_region2, lng_region),
    lat_region = as.numeric(lat_region),
    lng_region = as.numeric(lng_region)
  ) %>%
  dplyr::select(-lng_region2) %>%  
  # Extract lat/lng from Loc_exact
  separate(Loc_exact, into = c("lat_exact", "lng_exact"), sep = ", ", extra = "merge", remove = FALSE) %>%
  separate(lat_exact, into = c("lat_exact", "lng_exact2"), sep = ",", extra = "merge", remove = TRUE) %>%
  mutate(
    lng_exact = ifelse(is.na(lng_exact), lng_exact2, lng_exact),
    lat_exact = as.numeric(lat_exact),
    lng_exact = as.numeric(lng_exact)
  ) %>%
  dplyr::select(-lng_exact2)  

# --------------------------------
# Cluster Group_region within 3km
# --------------------------------
# Step 1: Extract Unique Region Coordinates (ignoring NA)
unique_regions <- data %>%
  filter(!is.na(lat_region) & !is.na(lng_region)) %>%
  distinct(lat_region, lng_region)
# Step 2: Convert to sf Spatial Points (WGS84 CRS)
sf_regions <- unique_regions %>%
  st_as_sf(coords = c("lng_region", "lat_region"), crs = 4326)
# Step 3: Compute Distance Matrix & Cluster Points Within 3 km
dist_matrix <- st_distance(sf_regions)  # Distance between all points
clustering <- hclust(as.dist(dist_matrix), method = "complete")  
unique_regions$cluster <- cutree(clustering, h = 3000)  # 3 km clustering
# Step 4: Calculate Mean Coordinates for Each Cluster
clustered_regions <- unique_regions %>%
  group_by(cluster) %>%
  summarise(lat_region_new = mean(lat_region),
            lng_region_new = mean(lng_region)) %>%
  ungroup()
# Step 5: Create a Mapping DataFrame (Old → New Coordinates)
region_mapping <- unique_regions %>%
  left_join(clustered_regions, by = "cluster") %>%
  dplyr::select(lat_region, lng_region, lat_region_new, lng_region_new)
# Step 6: Replace Old Coordinates in Original Data
data <- data %>%
  left_join(region_mapping, by = c("lat_region", "lng_region")) %>%
  mutate(lat_region = ifelse(!is.na(lat_region_new), lat_region_new, lat_region),
         lng_region = ifelse(!is.na(lng_region_new), lng_region_new, lng_region)) %>%
  dplyr::select(-lat_region_new, -lng_region_new)

# --------------------------------
# Cluster Group_region within 75m
# --------------------------------
# Step 1: Extract Unique Region Coordinates (ignoring NA)
unique_regions <- data %>%
  filter(!is.na(lat_exact) & !is.na(lng_exact)) %>%
  distinct(lat_exact, lng_exact)
# Step 2: Convert to sf Spatial Points (WGS84 CRS)
sf_regions <- unique_regions %>%
  st_as_sf(coords = c("lng_exact", "lat_exact"), crs = 4326)
# Step 3: Compute Distance Matrix & Cluster Points Within 75m
dist_matrix <- st_distance(sf_regions)  # Distance between all points
clustering <- hclust(as.dist(dist_matrix), method = "complete")  
unique_regions$cluster <- cutree(clustering, h = 75)  # 75m clustering
# Step 4: Calculate Mean Coordinates for Each Cluster
clustered_regions <- unique_regions %>%
  group_by(cluster) %>%
  summarise(lat_exact_new = mean(lat_exact),
            lng_exact_new = mean(lng_exact)) %>%
  ungroup()
# Step 5: Create a Mapping DataFrame (Old → New Coordinates)
region_mapping <- unique_regions %>%
  left_join(clustered_regions, by = "cluster") %>%
  dplyr::select(lat_exact, lng_exact, lat_exact_new, lng_exact_new)
# Step 6: Replace Old Coordinates in Original Data
data <- data %>%
  left_join(region_mapping, by = c("lat_exact", "lng_exact")) %>%
  mutate(lat_exact = ifelse(!is.na(lat_exact_new), lat_exact_new, lat_exact),
         lng_exact = ifelse(!is.na(lng_exact_new), lng_exact_new, lng_exact)) %>%
  dplyr::select(-lat_exact_new, -lng_exact_new)

######################### Site-ID #######################################

data <- data %>% mutate(lng_tmp = lng_exact, lat_tmp = lat_exact) %>%
  mutate(lng_tmp = ifelse(is.na(lng_tmp), lng_region, lng_tmp),
         lat_tmp = ifelse(is.na(lat_tmp), lat_region, lat_tmp)) %>%
  group_by(lng_tmp, lat_tmp) %>%
    mutate(Index_tmp = cur_group_id()) %>%
    ungroup() %>%
  mutate(Site_ID = paste("Site", Country, Index_tmp, sep= "_")) %>%
  dplyr::select(-lng_tmp, -lat_tmp, -Index_tmp)