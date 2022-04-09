library(readr)

# make this the path to where the data are saved
data_dir <- # e.g.: '/home/Data/'
# make this the path to where the R scripts are saved
script_dir <- # e.g.:  '/home/Scripts/'
# make this the path to where the plots should be saved
# plot_dir <- # directory you want to save the plot out to
plot_dir <- # e.g.:  '/home/Plots/'
  
# import our functions to make the wordclouds
# A modification & extension of the approach outlined here:
# https://methodmatters.github.io/wordclouds-for-management-presentations/
source(paste0(script_dir, "word_cloud_functions.R"))

# load the raw data
# a subset of the more complete dataset
raw_data <- read_csv(paste0(data_dir, 'omnibus_jobs_subset_r.csv'))

### Define the color palette so that it matches the colors used in Python / Seaborn
# analyst, engineer, scientist, ml engineer
role_palette <- c(rgb(0.3333333333333333, 0.6588235294117647, 0.40784313725490196),
                  rgb(0.8666666666666667, 0.5176470588235295, 0.3215686274509804),
                  rgb(0.2980392156862745, 0.4470588235294118, 0.6901960784313725),
                  rgb(0.7686274509803922, 0.3058823529411765, 0.3215686274509804))

############# English Language Word Cloud
############# English Language Word Cloud
############# English Language Word Cloud
############# English Language Word Cloud

# make document-feature matrix of raw text
dfm_jd_raw = master_cleaning_function(data_f = raw_data %>% filter(language == 'en'),
                                      language_f = 'english',  
                                      text_field_f = 'job_description_text',
                                        dfm_method_f = 'doc_freq')


# comparison cloud matrix - no word corrections
cc_jd <- make_comparison_cloud_category(input_data_f = dfm_jd_raw,
                                        grouping_cat_f = 'job_function')

# look at the wordcloud - what needs to be "cleaned up?"
# what is redundant or unclear and can go?
comparison.cloud(cc_jd, max.words = 50, scale = c(1.5, 1), 
                 title.size = 2, colors = role_palette, 
                 rot.per = 0, main="Title")


# define columns we want to replace (old_cols)
# the cols we want to put in (new_cols)
# and the words that are redundant with other 
# words that already appear and so should not be shown (words_to_remove)
old_cols <- c('data_warehous', 'data_pipelin', 'engin', 'analys', 'identifi', 'analyt', 'analysi',
              'statist', 'deep_learn', 'machin_learn', 'comput', 'softwar', 'infrastructur',
              'architectur',  'stakehold', 'softwar_engin', 'data_scienc',
              'softwar_develop', 'manag', 'comput_vision', 'comput_scienc', 'ensur', 'learn_model' ,
              'mathemat', 'report', 'data_analysi', 'abil', 'artifici_intellig', 'databas' )
new_cols <- c('data_warehouse', 'data_pipeline', 'engineer', 'analyse', 'identify', 'analytical', 'analysis',
              'statistics', 'deep_learning', 'machine_learning', 'computer', 'software', 'infrastructure',
              'architecture', 'stakeholder', 'software_engineering', 'data_science',
              'software_development', 'manage', 'computer_vision', 'computer_science', 'ensure', 'learning_model',
              'mathematics', 'reporting', 'data_analysis', 'ability', 'artificial_intelligence', 'database')
words_to_remove <- c('data_engin',  'pipelin', 'warehous', 'scienc','scienc', 
                     'deep', 'learn', 'machin', 'learn_engin', 'learn_framework', 'vision',
                     'tensorflow_pytorch', 'analyst', 'scientist', 'data_analyst', 'data_scientist',
                     'artifici')

# make our cleaned dfm, making the word substitutions
# we defined above
dfm_jd_clean <- make_comparison_cloud_category(input_data_f = dfm_jd_raw,
                                        grouping_cat_f = 'job_function',
                                        words_to_remove_f = words_to_remove,
                                        old_cols_f = old_cols, 
                                        new_cols_f = new_cols)

# make the final plot.
# note - you might need to save the file to get the formatting just right
png(paste0(plot_dir,'comparison_cloud_en_data_jobs.png'),width=12/1.1,height=7.5/1.1, units='in', res=300)
layout(matrix(c(1, 2), nrow=2), heights=c(.5, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Comparison Cloud: English Language Job Descriptions", cex = 2)
mtext(c("Words that are more distinct for a category appear in larger print"), side=1, line=0, font =3, col=c("black"))
mtext(c("(N = 3,869)"), side=1, line=29, at = 0, font = 3, col=c("black"))
comparison.cloud(dfm_jd_clean, max.words = 50, scale = c(2.4, .5), 
                 title.size = 1.5, colors = role_palette,
                 rot.per = 0)
dev.off()

# remove objects from memory that we created for the English word clouds
rm(cc_jd)
rm(dfm_jd_raw)
rm(pow_texts)
rm(dfm_jd_clean)
rm(new_cols)
rm(old_cols)
rm(words_to_remove)


############ French Language Word Cloud
############ French Language Word Cloud
############ French Language Word Cloud
############ French Language Word Cloud
############ French Language Word Cloud


# make document-feature matrix of raw text
dfm_jd_raw_fr = master_cleaning_function(data_f = raw_data %>% filter(language == 'fr') %>%
                                      # remove weird d' that quanteda doesn't seem to get
                                      mutate(job_description_text = gsub("d'", "", job_description_text)),
                                      language_f = 'french',  
                                      text_field_f = 'job_description_text',
                                      dfm_method_f = 'doc_freq')

# comparison cloud matrix - no word corrections
cc_jd_fr <- make_comparison_cloud_category(input_data_f = dfm_jd_raw_fr,
                                        grouping_cat_f = 'job_function')


# define columns we want to replace (old_cols)
# the cols we want to put in (new_cols)
# and the words that are redundant with other 
# words that already appear and so should not be shown (words_to_remove)
old_cols_fr <- c('kafk', 'agil', 'scal', 'analys', 
                  'prédict', 'appliqu', 'remot',
                 'stockag','big_dat', 'statist', 'automatis', 'salair',
                 'techniqu', 'développ_nouveau', 'algorithm', 'cadr_typ', 
                 'pipelin', 'méti',  'visualis', 'stag', 'automat', 
                 'innov', 'perform', 'recherch', 'architectur', 'plateform',
                 'azur',  'apprentissag', 'machin_learning','dat_scienc',
                 'intelligent_artificiel', 'model')
new_cols_fr <- c('kafka', 'agile', 'scala', 'analyse', 
               'prédiction', 'appliquer', 'remote',
                  'stockage', 'big_data', 'statistique', 'automatiser', 'salaire',
                 'technique', 'développer_nouveau', 'algorithme', 'cadre', 
                 'pipeline', 'métier', 'visualisation', 'stage', 'automatique', 
                 'innovation', 'performance', 'recherche', 'architecture', 'plateforme',
                 'azure', 'apprentissage', 'machine_learning', 'data_science',
                 'intelligence_artificielle', 'modèle')
words_to_remove_fr <- c( 'scientist', 'deep', 'learning', 'big', 'machin',
                         'dat_scientist', 'dat_engine', 'dat_analyst',  'scienc',
                         'analyst', 'tableau', 'bord', 'pow', 'des', 'rejoign', 
                         'accompl', 'engine', 'alor',  
                         'dat', 'point', 'ains', 'artificiel')

# make our cleaned dfm, making the word substitutions
# we defined above
dfm_jd_clean_fr <- make_comparison_cloud_category(input_data_f = dfm_jd_raw_fr,
                                               grouping_cat_f = 'job_function',
                                               words_to_remove_f = words_to_remove_fr,
                                               old_cols_f = old_cols_fr, 
                                               new_cols_f = new_cols_fr)


# make the final plot.
# note - you might need to save the file to get the formatting just right
png(paste0(plot_dir,'comparison_cloud_fr_data_jobs.png'),width=12/1.1,height=7.5/1.1, units='in', res=300)
layout(matrix(c(1, 2), nrow=2), heights=c(.5, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Comparison Cloud: French Language Job Descriptions", cex = 2)
mtext(c("Words that are more distinct for a category appear in larger print"), side=1, line=0, font =3, col=c("black"))
mtext(c("(N = 1,349)"), side=1, line=29, at = 0, font = 3, col=c("black"))
comparison.cloud(dfm_jd_clean_fr, max.words = 45, scale = c(2.5, .5), 
                 title.size = 1.5, colors = role_palette,
                 rot.per = 0)
dev.off()
