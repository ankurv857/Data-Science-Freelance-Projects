


#Initializing libraries

install.packages("readr")
install.packages("xgboost")
install.packages("sqldf")
install.packages("data.table")
install.packages("dplyr")
install.packages("SparseM")
install.packages("SnowballC")
install.packages("tm")
install.packages("e1071")
install.packages("caTools")
install.packages("NLP")


library('readr')
library('xgboost')
library('sqldf')
library('data.table')
library('dplyr')
library('SparseM')
library('SnowballC')
library('tm')
library('e1071')
library('caTools')
library('NLP')

#Keep the files at respective location and set the directory accordingly

getwd()
setwd("/Users/ankur/Documents/Upworks/")

#-------------------------------------Ignore This Part------------------------------------------------------------#
#Thinknook Data Twitter
think = fread("Thinknook.csv")
think = data.frame(think)
think = think[c(2,4)]
colnames(think)  = c("Emotion", "Lower")
sample1 = think[grepl("@", think$Lower), ]
sample2 = (sample.split(sample1$Emotion, SplitRatio=0.5))
sample3 = sample1[ sample2,]

#Movie Survey Data
survey =  fread("Survey.csv")

data = rbind.data.frame(survey,sample3)
write.csv(data,"Train.csv",row.names = FALSE)

#-------------------------------------Start From Here------------------------------------------------------------#

#Train Data
data = fread("Train.csv")

#Test Data
test1= read.csv("Test2.csv")
test2= read.csv("Test.csv")

test = rbind(test1,test2)

# Processing train File

TextProcess<- function(x){
  z<-Corpus(VectorSource(x))
  z<-tm_map(z, content_transformer(tolower))
  z<-tm_map(z, PlainTextDocument)
  z<-tm_map(z, removePunctuation)
  z<-tm_map(z, removeWords,stopwords("english"))
  z<-tm_map(z, stemDocument)
  sapply(1:length(x), function(y) as.character(z[[y]][[1]]))
}


profile<-as.character(data$Lower)
profile_clean = TextProcess(profile)

corpus_s <- Corpus(VectorSource(profile_clean))
f_s <-DocumentTermMatrix(corpus_s,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
sparse_s <- removeSparseTerms(f_s, 0.99925)
dim(f_s)
dim(sparse_s)
dtm_train<-as.data.frame(as.matrix(sparse_s))
dim(dtm_train)

train1 = data

train1[grep("abilities|ability|aboard|absolve|absolved|absolves|absolving|absorbed|accept|accepted|accepting|accepts|accomplish|accomplished|accomplishes|achievable|acquit|acquits|acquitted|acquitting|active|adequate|admire|admired|admires|admiring|adopt|adopts|adorable|adore|adored|adores|advanced|advantage|advantages|adventure|adventures|adventurous|affection|affectionate|agog|agree|agreeable|agreed|agreement|agrees|alive|allow|amaze|amazed|amazes|amazing|ambitious|amuse|amused|amusement|amusements|anticipation|appease|appeased|appeases|appeasing|applaud|applauded|applauding|applauds|applause|appreciate|appreciated|appreciates|appreciating|appreciation|
            approval|approved|approves|ardent|asset|assets|astonished|astound|astounded|astounding|astoundingly|astounds|attract|attracted|attracting|attraction|attractions|attracts|audacious|authority|avid|award|awarded|awards|awesome|backed|backing|backs|bargain|beatific|beauties|beautiful|beautifully|beautify|beloved|benefit|benefits|benefitted|benefitting|best|better|big|bless|blesses|blessing|bliss|blissful|blithe|blockbuster|bold|boldly|boost|boosted|boosting|boosts|brave|breakthrough|breathtaking|bright|brightest|brightness|brilliant|brisk|buoyant|calm|calmed|calming|calms|capable|captivated|
            care|carefree|careful|carefully|cares|celebrate|celebrated|celebrates|celebrating|certain|chance|chances|charm|charming|cheer|cheered|cheerful|cheering|cheers|cheery|cherish|cherished|cherishes|cherishing|chic|clarifies|clarity|classy|clean|cleaner|clear|cleared|clearly|clears|clever|comedy|comfort|comfortable|comforting|comforts|commend|commended|commit|commitment|commits|committed|committing|compassionate|compelled|competent|competitive|comprehensive|conciliate|conciliated|conciliates|conciliating|confidence|confident|congrats|congratulate|congratulation|congratulations|consent|consents|consolable|convince|convinced|
            convinces|convivial|cool|cool stuff|courage|courageous|courteous|courtesy|coziness|creative|curious|cute|daredevil|daring|dauntless|dear|dearly|debonair|decisive|dedicated|defender|defenders|delight|delighted|delighting|delights|desirable|desire|desired|desirous|determined|devoted|diamond|dream|dreams|eager|earnest|ease|easy|ecstatic|effective|effectively|elated|elation|elegant|elegantly|embrace|empathetic|enchanted|encourage|encouraged|encouragement|encourages|endorse|endorsed|endorsement|endorses|energetic|engage|engages|engrossed|enjoy|enjoying|enjoys|enlighten|enlightened|enlightening|enlightens|enrapture|ensure|
            ensuring|enterprising|entertaining|enthral|enthusiastic|entitled|entrusted|esteemed|ethical|euphoria|euphoric|exasperated|excellence|excellent|excite|excited|excitement|exciting|exclusive|exhilarated|exhilarates|exhilarating|exonerate|exonerated|exonerates|exonerating|expand|expands|exploration|explorations|extend|extends|exuberant|exultant|exultantly|fabulous|fair|faith|faithful|fame|fan|fantastic|fascinate|fascinated|fascinates|fascinating|favor|favored|favorite|favorited|favorites|favors|fearless|feeling|fervent|fervid|festive|fine|fit|fitness|flagship|focused|fond|fondness|forgive|forgiving|fortunate|free|freedom|fresh|friendly|
            frisky|fulfill|fulfilled|fulfills|fun|funky|funnier|funny|futile|gain|gained|gaining|gains|gallant|gallantly|gallantry|generous|genial|gift|glad|glamorous|glamourous|glee|gleeful|glorious|glory|god|godsend|good|goodness|grace|gracious|grand|grant|granted|granting|grants|grateful|gratification|great|greater|greatest|greet|greeted|greeting|greetings|greets|growing|growth|guarantee|ha|haha|hahaha|hahahah|hail|hailed|happiness|happy|hardier|hardy|haunting|healthy|heartfelt|heaven|heavenly|help|helpful|helping|helps|hero|heroes|heroic|highlight|hilarious|honest|honor|honored|honoring|honour|honoured|honouring|hope|hopeful|hopefully|hopes|
            hoping|hug|huge|hugs|humerous|humor|humorous|humour|humourous|hurrah|immortal|immune|importance|important|impress|impressed|impresses|impressive|improve|improved|improvement|improves|improving|increase|increased|indestructible|infatuated|infatuation|influential|innovate|innovates|innovation|innovative|inquisitive|inspiration|inspirational|inspire|inspired|inspires|inspiring|intact|integrity|intelligent|intense|interest|interested|interesting|interests|intricate|intrigues|invincible|invite|inviting|invulnerable|irresistible|irresponsible|jaunty|jesus|jewel|jewels|jocular|join|joke|jokes|jolly|jovial|joy|joyful|joyfully|joyous|jubilant|justice|justifiably|
            justified|keen|kind|kinder|kiss|kudos|landmark|laugh|laughed|laughing|laughs|laughting|launched|lawl|legal|legally|lenient|lifesaver|lighthearted|like|liked|likes|lively|lmao|lmfao|lol|lovable|love|loved|lovelies|lovely|loving|loyal|loyalty|luck|luckily|lucky|marvel|marvelous|marvels|masterpiece|masterpieces|matter|matters|mature|meaningful|medal|meditative|mercy|merry|methodical|miracle|mirth|mirthful|mirthfully|motivate|motivated|motivating|motivation|natural|nice|nifty|noble|novel|obsessed|oks|ominous|once-in-a-lifetime|opportunities|opportunity|optimism|optimistic|outreach|outstanding|overjoyed|paradise|pardon|pardoned|pardoning|pardons|passionate|peace|
            peaceful|peacefully|perfect|perfected|perfectly|perfects|picturesque|playful|pleasant|please|pleased|pleasure|popular|positive|positively|powerful|praise|praised|praises|praising|pray|praying|prays|prepared|pretty|privileged|proactive|progress|prominent|promise|promised|promises|promote|promoted|promotes|promoting|prospect|prospects|prosperous|protect|protected|protects|proud|proudly|rapture|raptured|raptures|rapturous|ratified|reach|reached|reaches|reaching|reassure|reassured|reassures|reassuring|recommend|recommended|recommends|redeemed|rejoice|rejoiced|rejoices|rejoicing|relaxed|reliant|relieve|relieved|relieves|relieving|relishing|remarkable|
            rescue|rescued|rescues|resolute|resolve|resolved|resolves|resolving|respected|responsible|responsive|restful|restore|restored|restores|restoring|revered|revive|revives|reward|rewarded|rewarding|rewards|rich|right direction|rigorous|rigorously|robust|rofl|roflcopter|roflmao|romance|rotfl|rotflmfao|rotflol|safe|safely|safety|salient|satisfied|save|saved|scoop|secure|secured|secures|self-confident|serene|sexy|share|shared|shares|significance|significant|sincere|sincerely|sincerest|sincerity|slick|slicker|slickest|smart|smarter|smartest|smile|smiled|smiles|smiling|sobering|solid|solidarity|solution|solutions|solve|solved|solves|solving",          
            train1$Lower, ignore.case = TRUE), "Positive"] = 1





train1[grep("abandon|abandoned|abandons|abducted|abduction|abductions|abhor|abhorred|abhorrent|abhors|absentee|absentees|abuse|abused|abuses|abusive|accident|accidental|accidentally|accidents|accusation|accusations|accuse|accused|accuses|accusing|ache|aching|acrimonious|admit|admits|admitted|admonish|admonished|affected|afflicted|affronted|afraid|aggravate|aggravated|aggravates|aggravating|aggression|aggressions|aggressive|aghast|agonise|agonised|agonises|agonising|agonize|agonized|agonizes|agonizing|alarm|alarmed|alarmist|alarmists|alas|alert|alienation|allergic|alone|ambivalent|anger|angers|angry|anguish|anguished|animosity|annoy|annoyance|annoyed|annoying|annoys|antagonistic|anti|anxiety|anxious|
            apathetic|apathy|apocalyptic|apologise|apologised|apologises|apologising|apologize|apologized|apologizes|apologizing|apology|appalled|appalling|apprehensive|arrest|arrested|arrests|arrogant|ashame|ashamed|assassination|assassinations|attack|attacked|attacking|attacks|avert|averted|averts|avoid|avoided|avoids|await|awaited|awaits|awful|awkward|axe|axed|bad|badly|bailout|bamboozle|bamboozled|bamboozles|ban|banish|bankrupt|bankster|banned|barrier|battle|battles|beaten|beating|belittle|belittled|bereave|bereaved|bereaves|bereaving|betray|betrayal|betrayed|betraying|betrays|bias|biased|bitter|bitterly|bizarre|blah|blame|blamed|blames|blaming|blind|block|blocked|blocking|blocks|bloody|blurry|
            boastful|bomb|bore|bored|boring|bother|bothered|bothers|bothersome|boycott|boycotted|boycotting|boycotts|brainwashing|bribe|broke|broken|brooding|bullied|bullshit|bully|bullying|bummer|burden|burdened|burdening|burdens|cant stand|cancel|cancelled|cancelling|cancels|cancer|careless|cashing in|casualty|catastrophe|catastrophic|cautious|censor|censored|censors|chagrin|chagrined|challenge|chaos|chaotic|charged|charges|charmless|chastise|chastised|chastises|chastising|cheat|cheated|cheater|cheaters|cheats|cheerless|childish|chilling|choke|choked|chokes|choking|clash|clouded|clueless|coerced|collapse|collapsed|collapses|collapsing|collide|collides|colliding|collision|collisions|colluding|combat|combats|complacent|complain|complained|complains|condemn|
            condemnation|condemned|condemns|conflict|conflicting|conflictive|conflicts|confuse|confused|confusing|conspiracy|constrained|contagion|contagions|contagious|contempt|contemptuous|contemptuously|contend|contender|contending|contentious|contestable|controversial|controversially|cornered|corpse|costly|cover-up|coward|cowardly|cramp|crap|crash|crazier|craziest|crazy|crestfallen|cried|cries|crime|criminal|criminals|crisis|critic|criticism|criticize|criticized|criticizes|criticizing|critics|cruel|cruelty|crush|crushed|crushes|crushing|cry|crying|curse|cut|cuts|cutting|cynic|cynical|cynicism|damage|damages|damn|damned|damnit|danger|darkest|darkness|dead|deadlock|deafening|death|debt|deceit|deceitful|deceive|deceived|deceives|deceiving|deception|defeated|defect|defects|defenseless|defer|deferring|defiant|deficit|degrade|degraded|degrades|dehumanize|dehumanized|dehumanizes|dehumanizing|deject|dejected|dejecting|dejects|delay|delayed|demand|demanded|demanding|demands|demonstration|demoralized|denied|denier|deniers|denies|denounce|denounces|deny|denying|depressed|
            depressing|derail|derailed|derails|deride|derided|derides|deriding|derision|despair|despairing|despairs|desperate|desperately|despondent|destroy|destroyed|destroying|destroys|destruction|destructive|detached|detain|detained|detention|devastate|devastated|devastating|die|died|difficult|diffident|dilemma|dipshit|dire|direful|dirt|dirtier|dirtiest|dirty|disabling|disadvantage|disadvantaged|disappear|disappeared|disappears|disappoint|disappointed|disappointing|disappointment|disappointments|disappoints|disaster|disasters|disastrous|disbelieve|discard|discarded|discarding|discards|disconsolate|disconsolation|discontented|discord|discounted|discouraged|discredited|disdain|disgrace|disgraced|disguise|disguised|disguises|disguising|disgust|disgusted|disgusting|disheartened|dishonest|disillusioned|disinclined|disjointed|dislike|dismal|dismayed|disorder|disorganized|disoriented|disparage|disparaged|disparages|disparaging|displeased|dispute|disputed|disputes|disputing|disqualified|disquiet|disregard|disregarded|disregarding|disregards|disrespect|disrespected|disruption|disruptions|disruptive|dissatisfied|distort|distorted|distorting|distorts|distract|distracted|distraction|distracts|distress|distressed|distresses|distressing|distrust|distrustful|disturb|disturbed|disturbing|disturbs|dithering|dizzy|dodging|dodgy|does not work|dolorous|dont like|doom|doomed|doubt|doubted|doubtful|doubting|doubts|downcast|downhearted|downside|drag|dragged|drags|drained|dread|dreaded|dreadful|dreading|dreary|droopy|drop|drown|drowned|drowns|drunk|dubious|dud|dull|dumb|dump|dumped|dumps|dupe|duped|dysfunction|eerie|eery|embarrass|embarrassed|embarrasses|embarrassing|embarrassment|embittered|emergency|emptiness|empty|enemies|enemy|ennui|enrage|enraged|enrages|enraging|enslave|enslaved|enslaves|envies|envious|envy|envying|erroneous|error|errors|escape|escapes|escaping|eviction|evil|exaggerate|exaggerated|exaggerates|exaggerating|exclude|excluded|exclusion|excuse|exempt|exhausted|expel|expelled|expelling|expels|exploit|exploited|exploiting|exploits|expose|exposed|exposes|exposing|fad|fail|failed|failing|fails|failure|failures|fainthearted|fake|fakes|faking|fallen|falling|falsified|falsify|farce|fascist|fascists|fatalities|fatality|fatigue|fatigued|fatigues|fatiguing|fear|fearful|fearing|fearsome|fed up|feeble|felonies|felony|fiasco|fidgety|fight|fire|fired|firing|flees|flop|flops|flu|flustered|fool|foolish|fools|forced|foreclosure|foreclosures|forget|forgetful|forgotten|frantic|fraud|frauds|fraudster|fraudsters|fraudulence|fraudulent|frenzy|fright|frightened|frightening|frikin|frowning|frustrate|frustrated|frustrates|frustrating|frustration|fud|fuming|funeral|funerals|furious|gag|gagged|ghost|giddy|gloom|gloomy|glum|goddamn|grave|gray|greed|greedy|green wash|green washing|greenwash|greenwasher|greenwashers|greenwashing|grey|grief|grieved|gross|guilt|guilty|gullibility|gullible|gun|hacked|hapless|haplessness|hard|hardship|harm|harmed|harmful|harming|harms|harried|harsh|harsher|harshest|hate|hated|haters|hates|hating|haunt|haunted|haunts|havoc|heartbreaking|heartbroken|heavyhearted|hell|helpless|hesitant|hesitate|hid|hide|hides|hiding|hindrance|hoax|homesick|hooligan|hooliganism|hooligans|hopeless|hopelessness|horrendous|horrible|horrific|horrified|hostile|huckster|humiliated|humiliation|hunger|hurt|hurting|hurts|hypocritical|hysteria|hysterical|hysterics|idiot|idiotic|ignorance|ignorant|ignore|ignored|ignores|ill|illegal|illiteracy|illness|illnesses|imbecile|immobilized|impatient|imperfect|impose|imposed|imposes|imposing|impotent|imprisoned|inability|inaction|inadequate|incapable|incapacitated|incensed|incompetence|incompetent|inconsiderate|inconvenience|inconvenient|indecisive|indifference|indifferent|indignant|indignation|indoctrinate|indoctrinated|indoctrinates|indoctrinating|ineffective|ineffectively|infected|inferior|inflamed|infringement|infuriate|infuriated|infuriates|infuriating|inhibit|injured|
            injury|injustice|inquisition|insane|insanity|insecure|insensitive|insensitivity|insignificant|insipid|insult|insulted|insulting|insults|interrogated|interrupt|interrupted|interrupting|interruption|interrupts|intimidate|intimidated|intimidates|intimidating|
            intimidation|irate|ironic|irony|irrational|irresolute|irreversible|irritate|irritated|irritating|isolated|itchy|jailed|jealous|jeopardy|joyless|jumpy|kill|killed|killing|kills|lack|lackadaisical|lag|lagged|lagging|lags|lame|lawsuit|lawsuits|lazy|leak|leaked|leave|lethargic|lethargy|liar|liars|libelous|lied|limitation|limited|limits|litigation|litigious|livid|loathe|loathed|loathes|loathing|lobby|lobbying|lonely|lonesome|longing|loom|loomed|looming|looms|loose|looses|loser|losing|loss|lost|lowest|lugubrious|lunatic|lunatics|lurk|lurking|lurks|mad|maddening|made-up|madly|madness|mandatory|manipulated|manipulating|manipulation|meaningless|mediocrity|melancholy|menace|menaced|mess|messed|messing up|mindless|misbehave|misbehaved|misbehaves|misbehaving|mischief|mischiefs|miserable|misery|misgiving|misinformation|misinformed|misinterpreted|misleading|misread|misreporting|misrepresentation|miss|missed|missing|mistake|mistaken|mistakes|mistaking|misunderstand|misunderstanding|misunderstands|misunderstood|moan|moaned|moaning|moans|mock|mocked|mocking|mocks|mongering|monopolize|monopolized|monopolizes|monopolizing|moody|mope|moping|moron|mourn|mourned|mournful|mourning|mourns|mumpish|murder|murderer|murdering|murderous|murders|myth|n00b|naive|nasty|nave|needy|negative|negativity|neglect|neglected|neglecting|neglects|nerves|nervous|nervously|no|no fun|noisy|nonsense|noob|nosey|not good|not working|notorious|numb|nuts|obliterate|obliterated|obnoxious|obscene|obsolete|obstacle|obstacles|obstinate|odd|offend|offended|offender|offending|offends|offline|oppressed|oppressive|optionless|outcry|outmaneuvered|outrage|outraged|overload|overlooked|overreact|overreacted|overreaction|overreacts|oversell|overselling|oversells|oversimplification|oversimplified|oversimplifies|oversimplify|overstatement|overstatements|overweight|oxymoron|pain|pained|panic|panicked|panics|paradox|parley|passive|passively|pathetic|pay|penalty|pensive|peril|perjury|perpetrator|perpetrators|perplexed|persecute|persecuted|persecutes|persecuting|perturbed|pesky|pessimism|pessimistic|petrified|phobic|pileup|pique|piqued|piss|pissed|pissing|piteous|pitied|pity|poised|poison|poisoned|poisons|pollute|polluted|polluter|polluters|pollutes|poor|poorer|poorest|possessive|postpone|postponed|postpones|postponing|poverty|powerless|prblm|prblms|pressure|pressured|pretend|pretending|pretends|prevent|prevented|preventing|prevents|prison|prisoner|prisoners|problem|problems|profiteer|propaganda|prosecute|prosecuted|prosecutes|prosecution|protest|protesters|protesting|protests|provoke|provoked|provokes|provoking|pseudoscience|punish|punished|punishes|punitive|pushy|puzzled|quaking|questionable|questioned|questioning|racism|racist|racists|rage|rageful|rainy|rant|ranter|ranters|rants|rash|rebellion|recession|reckless|refuse|refused|refusing|regret|regretful|regrets|regretted|regretting|reject|rejected|rejecting|rejects|relentless|remorse|repulse|repulsed|resentful|resign|resigned|resigning|resigns|restless|restrict|restricted|restricting|restriction|restricts|retained|retard|retarded|retreat|revenge|revengeful|ridiculous|rig|rigged|riot|riots|risk|risks|rob|robber|robed|robing|robs|ruin|ruined|ruining|ruins|sabotage|sad|sadden|saddened|sadly|sappy|sarcastic|scam|scams|scandal|scandalous|scandals|scapegoat|scapegoats|scare|scared|scary|sceptical|scold|scorn|scornful|scream|screamed|screaming|screams|screwed|sedition|seditious|seduced|self-deluded|selfish|selfishness|sentence|sentenced|sentences|sentencing|severe|shaky|shame|shamed|shameful|shattered|shock|shocked|shocking|shocks|shoot|short-sighted|short-sightedness|shortage|shortages|shy|sick|sigh|silencing|silly|sinful|singleminded|skeptic|skeptical|skepticism|skeptics|slam|slash|slashed|slashes|slashing|slavery|sleeplessness|sluggish|smear|smog|sneaky|snub|snubbed|snubbing|snubs|solemn|somber|sore|sorrow|sorrowful|sorry|spam|spammer|spammers|spamming|speculative|
            spiritless|spiteful|squelched|stab|stabbed|stabs|stall|stalled|stalling|stampede|startled|starve|starved|starves|starving|steal|steals|stereotype|stereotyped|stifled|stingy|stolen|stop|stopped|stopping|stops|strange|strangely|strangled|stressed|stressor|stressors|stricken|strike|strikers|strikes|struck|struggle|struggled|struggles|struggling|stubborn|stuck|stunned|stupid|stupidly|subversive|sucks|suffer|suffering|suffers|suicidal|suicide|suing|sulking|sulky|sullen|suspect|suspected|suspecting|suspects|suspend|suspended|suspicious|swear|swearing|swears|swindle|swindles|swindling|tard|tears|tense|tension|terrible|terribly|terrified|terror|terrorize|terrorized|terrorizes|thorny|thoughtless|threat|threaten|threatened|threatening|threatens|threats|thwart|thwarted|thwarting|thwarts|timid|timorous|tired|tits|toothless|torn|torture|tortured|tortures|torturing|totalitarian|totalitarianism|tout|touted|touting|touts|tragedy|tragic|trap|trapped|trauma|traumatic|travesty|treason|treasonous|trembling|tremulous|tricked|trickery|trouble|troubled|troubles|tumor|ugly|unacceptable|unappreciated|unapproved|unaware|unbelievable|unbelieving|uncertain|unclear|uncomfortable|unconcerned|unconfirmed|unconvinced|uncredited|undecided|underestimate|underestimated|underestimates|underestimating|undermine|undermined|undermines|undermining|undeserving|undesirable|uneasy|unemployment|unequal|unethical|unfair|unfocused|unfulfilled|unhappy|unhealthy|unimpressed|unintelligent|unjust|unlovable|unloved|unmotivated|unprofessional|unresearched|unsatisfied|unsecured|unsettled|unsophisticated|unstable|unsupported|unsure|unwanted|unworthy|upset|upsets|upsetting|uptight|urgent|useless|uselessness|vague|verdict|verdicts|vexation|vexing|vicious|victim|victimize|victimized|victimizes|victimizing|victims|vile|violate|violated|violates|violating|violence|violent|virulent|vitriolic|vociferous|vulnerability|vulnerable|walkout|walkouts|wanker|war|warfare|warn|warned|warning|warnings|warns|waste|wasted|wasting|wavering|weak|weakness|weary|weep|weeping|weird|whitewash|wicked|widowed|withdrawal|woebegone|woeful|worn|worried|worry|worrying|worse|worsen|worsened|worsening|worsens|worst|worthless|wrathful|wreck|wrong|wronged|yucky|zealot|zealots|not|dont",
            train1$Lower, ignore.case = TRUE), "Negative"] =  1

train1[is.na(train1)] = 0 

train = data.frame(dtm_train ,Positive =  train1$Positive , Negative = train1$Negative)
train_dep = data.frame(dep = data$Emotion , train)

#XGBoost Model
xgtrain <- xgb.DMatrix(data.matrix(train),label= data$Emotion )
param <- list(objective="binary:logistic", booster="gblinear",eta=.01,max_depth=6 ,subsample=0.77, colsample_by_tree=0.83,eval_metric = "auc")
xg_train_fit <- xgboost(data=xgtrain, param=param, nrounds=2000, verbose= TRUE,prediction=TRUE, maximize = TRUE)


#naive bayes
model <- naiveBayes(dep ~ ., data = train_dep)

# Processing test File

TextProcess<- function(x){
  z<-Corpus(VectorSource(x))
  z<-tm_map(z, content_transformer(tolower))
  z<-tm_map(z, PlainTextDocument)
  z<-tm_map(z, removePunctuation)
  z<-tm_map(z, removeWords,stopwords("english"))
  z<-tm_map(z, stemDocument)
  sapply(1:length(x), function(y) as.character(z[[y]][[1]]))
}

profile1<-as.character(test$Lower)
profile_clean1 = TextProcess(profile1)

corpus_s1 <- Corpus(VectorSource(profile_clean1))
f_s1 <-DocumentTermMatrix(corpus_s1,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
sparse_s1 <- removeSparseTerms(f_s1, 0.99925)
dim(f_s1)
dim(sparse_s1)
dtm_test<-as.data.frame(as.matrix(sparse_s1))

#Manually interpreting words from the strings

test1 = test

test1[grep("abilities|ability|aboard|absolve|absolved|absolves|absolving|absorbed|accept|accepted|accepting|accepts|accomplish|accomplished|accomplishes|achievable|acquit|acquits|acquitted|acquitting|active|adequate|admire|admired|admires|admiring|adopt|adopts|adorable|adore|adored|adores|advanced|advantage|advantages|adventure|adventures|adventurous|affection|affectionate|agog|agree|agreeable|agreed|agreement|agrees|alive|allow|amaze|amazed|amazes|amazing|ambitious|amuse|amused|amusement|amusements|anticipation|appease|appeased|appeases|appeasing|applaud|applauded|applauding|applauds|applause|appreciate|appreciated|appreciates|appreciating|appreciation|
           approval|approved|approves|ardent|asset|assets|astonished|astound|astounded|astounding|astoundingly|astounds|attract|attracted|attracting|attraction|attractions|attracts|audacious|authority|avid|award|awarded|awards|awesome|backed|backing|backs|bargain|beatific|beauties|beautiful|beautifully|beautify|beloved|benefit|benefits|benefitted|benefitting|best|better|big|bless|blesses|blessing|bliss|blissful|blithe|blockbuster|bold|boldly|boost|boosted|boosting|boosts|brave|breakthrough|breathtaking|bright|brightest|brightness|brilliant|brisk|buoyant|calm|calmed|calming|calms|capable|captivated|
           care|carefree|careful|carefully|cares|celebrate|celebrated|celebrates|celebrating|certain|chance|chances|charm|charming|cheer|cheered|cheerful|cheering|cheers|cheery|cherish|cherished|cherishes|cherishing|chic|clarifies|clarity|classy|clean|cleaner|clear|cleared|clearly|clears|clever|comedy|comfort|comfortable|comforting|comforts|commend|commended|commit|commitment|commits|committed|committing|compassionate|compelled|competent|competitive|comprehensive|conciliate|conciliated|conciliates|conciliating|confidence|confident|congrats|congratulate|congratulation|congratulations|consent|consents|consolable|convince|convinced|
           convinces|convivial|cool|cool stuff|courage|courageous|courteous|courtesy|coziness|creative|curious|cute|daredevil|daring|dauntless|dear|dearly|debonair|decisive|dedicated|defender|defenders|delight|delighted|delighting|delights|desirable|desire|desired|desirous|determined|devoted|diamond|dream|dreams|eager|earnest|ease|easy|ecstatic|effective|effectively|elated|elation|elegant|elegantly|embrace|empathetic|enchanted|encourage|encouraged|encouragement|encourages|endorse|endorsed|endorsement|endorses|energetic|engage|engages|engrossed|enjoy|enjoying|enjoys|enlighten|enlightened|enlightening|enlightens|enrapture|ensure|
           ensuring|enterprising|entertaining|enthral|enthusiastic|entitled|entrusted|esteemed|ethical|euphoria|euphoric|exasperated|excellence|excellent|excite|excited|excitement|exciting|exclusive|exhilarated|exhilarates|exhilarating|exonerate|exonerated|exonerates|exonerating|expand|expands|exploration|explorations|extend|extends|exuberant|exultant|exultantly|fabulous|fair|faith|faithful|fame|fan|fantastic|fascinate|fascinated|fascinates|fascinating|favor|favored|favorite|favorited|favorites|favors|fearless|feeling|fervent|fervid|festive|fine|fit|fitness|flagship|focused|fond|fondness|forgive|forgiving|fortunate|free|freedom|fresh|friendly|
           frisky|fulfill|fulfilled|fulfills|fun|funky|funnier|funny|futile|gain|gained|gaining|gains|gallant|gallantly|gallantry|generous|genial|gift|glad|glamorous|glamourous|glee|gleeful|glorious|glory|god|godsend|good|goodness|grace|gracious|grand|grant|granted|granting|grants|grateful|gratification|great|greater|greatest|greet|greeted|greeting|greetings|greets|growing|growth|guarantee|ha|haha|hahaha|hahahah|hail|hailed|happiness|happy|hardier|hardy|haunting|healthy|heartfelt|heaven|heavenly|help|helpful|helping|helps|hero|heroes|heroic|highlight|hilarious|honest|honor|honored|honoring|honour|honoured|honouring|hope|hopeful|hopefully|hopes|
           hoping|hug|huge|hugs|humerous|humor|humorous|humour|humourous|hurrah|immortal|immune|importance|important|impress|impressed|impresses|impressive|improve|improved|improvement|improves|improving|increase|increased|indestructible|infatuated|infatuation|influential|innovate|innovates|innovation|innovative|inquisitive|inspiration|inspirational|inspire|inspired|inspires|inspiring|intact|integrity|intelligent|intense|interest|interested|interesting|interests|intricate|intrigues|invincible|invite|inviting|invulnerable|irresistible|irresponsible|jaunty|jesus|jewel|jewels|jocular|join|joke|jokes|jolly|jovial|joy|joyful|joyfully|joyous|jubilant|justice|justifiably|
           justified|keen|kind|kinder|kiss|kudos|landmark|laugh|laughed|laughing|laughs|laughting|launched|lawl|legal|legally|lenient|lifesaver|lighthearted|like|liked|likes|lively|lmao|lmfao|lol|lovable|love|loved|lovelies|lovely|loving|loyal|loyalty|luck|luckily|lucky|marvel|marvelous|marvels|masterpiece|masterpieces|matter|matters|mature|meaningful|medal|meditative|mercy|merry|methodical|miracle|mirth|mirthful|mirthfully|motivate|motivated|motivating|motivation|natural|nice|nifty|noble|novel|obsessed|oks|ominous|once-in-a-lifetime|opportunities|opportunity|optimism|optimistic|outreach|outstanding|overjoyed|paradise|pardon|pardoned|pardoning|pardons|passionate|peace|
           peaceful|peacefully|perfect|perfected|perfectly|perfects|picturesque|playful|pleasant|please|pleased|pleasure|popular|positive|positively|powerful|praise|praised|praises|praising|pray|praying|prays|prepared|pretty|privileged|proactive|progress|prominent|promise|promised|promises|promote|promoted|promotes|promoting|prospect|prospects|prosperous|protect|protected|protects|proud|proudly|rapture|raptured|raptures|rapturous|ratified|reach|reached|reaches|reaching|reassure|reassured|reassures|reassuring|recommend|recommended|recommends|redeemed|rejoice|rejoiced|rejoices|rejoicing|relaxed|reliant|relieve|relieved|relieves|relieving|relishing|remarkable|
           rescue|rescued|rescues|resolute|resolve|resolved|resolves|resolving|respected|responsible|responsive|restful|restore|restored|restores|restoring|revered|revive|revives|reward|rewarded|rewarding|rewards|rich|right direction|rigorous|rigorously|robust|rofl|roflcopter|roflmao|romance|rotfl|rotflmfao|rotflol|safe|safely|safety|salient|satisfied|save|saved|scoop|secure|secured|secures|self-confident|serene|sexy|share|shared|shares|significance|significant|sincere|sincerely|sincerest|sincerity|slick|slicker|slickest|smart|smarter|smartest|smile|smiled|smiles|smiling|sobering|solid|solidarity|solution|solutions|solve|solved|solves|solving",          
           test1$Lower, ignore.case = TRUE), "Positive"] = 1

test1[grep("abandon|abandoned|abandons|abducted|abduction|abductions|abhor|abhorred|abhorrent|abhors|absentee|absentees|abuse|abused|abuses|abusive|accident|accidental|accidentally|accidents|accusation|accusations|accuse|accused|accuses|accusing|ache|aching|acrimonious|admit|admits|admitted|admonish|admonished|affected|afflicted|affronted|afraid|aggravate|aggravated|aggravates|aggravating|aggression|aggressions|aggressive|aghast|agonise|agonised|agonises|agonising|agonize|agonized|agonizes|agonizing|alarm|alarmed|alarmist|alarmists|alas|alert|alienation|allergic|alone|ambivalent|anger|angers|angry|anguish|anguished|animosity|annoy|annoyance|annoyed|annoying|annoys|antagonistic|anti|anxiety|anxious|
           apathetic|apathy|apocalyptic|apologise|apologised|apologises|apologising|apologize|apologized|apologizes|apologizing|apology|appalled|appalling|apprehensive|arrest|arrested|arrests|arrogant|ashame|ashamed|assassination|assassinations|attack|attacked|attacking|attacks|avert|averted|averts|avoid|avoided|avoids|await|awaited|awaits|awful|awkward|axe|axed|bad|badly|bailout|bamboozle|bamboozled|bamboozles|ban|banish|bankrupt|bankster|banned|barrier|battle|battles|beaten|beating|belittle|belittled|bereave|bereaved|bereaves|bereaving|betray|betrayal|betrayed|betraying|betrays|bias|biased|bitter|bitterly|bizarre|blah|blame|blamed|blames|blaming|blind|block|blocked|blocking|blocks|bloody|blurry|
           boastful|bomb|bore|bored|boring|bother|bothered|bothers|bothersome|boycott|boycotted|boycotting|boycotts|brainwashing|bribe|broke|broken|brooding|bullied|bullshit|bully|bullying|bummer|burden|burdened|burdening|burdens|cant stand|cancel|cancelled|cancelling|cancels|cancer|careless|cashing in|casualty|catastrophe|catastrophic|cautious|censor|censored|censors|chagrin|chagrined|challenge|chaos|chaotic|charged|charges|charmless|chastise|chastised|chastises|chastising|cheat|cheated|cheater|cheaters|cheats|cheerless|childish|chilling|choke|choked|chokes|choking|clash|clouded|clueless|coerced|collapse|collapsed|collapses|collapsing|collide|collides|colliding|collision|collisions|colluding|combat|combats|complacent|complain|complained|complains|condemn|
           condemnation|condemned|condemns|conflict|conflicting|conflictive|conflicts|confuse|confused|confusing|conspiracy|constrained|contagion|contagions|contagious|contempt|contemptuous|contemptuously|contend|contender|contending|contentious|contestable|controversial|controversially|cornered|corpse|costly|cover-up|coward|cowardly|cramp|crap|crash|crazier|craziest|crazy|crestfallen|cried|cries|crime|criminal|criminals|crisis|critic|criticism|criticize|criticized|criticizes|criticizing|critics|cruel|cruelty|crush|crushed|crushes|crushing|cry|crying|curse|cut|cuts|cutting|cynic|cynical|cynicism|damage|damages|damn|damned|damnit|danger|darkest|darkness|dead|deadlock|deafening|death|debt|deceit|deceitful|deceive|deceived|deceives|deceiving|deception|defeated|defect|defects|defenseless|defer|deferring|defiant|deficit|degrade|degraded|degrades|dehumanize|dehumanized|dehumanizes|dehumanizing|deject|dejected|dejecting|dejects|delay|delayed|demand|demanded|demanding|demands|demonstration|demoralized|denied|denier|deniers|denies|denounce|denounces|deny|denying|depressed|
           depressing|derail|derailed|derails|deride|derided|derides|deriding|derision|despair|despairing|despairs|desperate|desperately|despondent|destroy|destroyed|destroying|destroys|destruction|destructive|detached|detain|detained|detention|devastate|devastated|devastating|die|died|difficult|diffident|dilemma|dipshit|dire|direful|dirt|dirtier|dirtiest|dirty|disabling|disadvantage|disadvantaged|disappear|disappeared|disappears|disappoint|disappointed|disappointing|disappointment|disappointments|disappoints|disaster|disasters|disastrous|disbelieve|discard|discarded|discarding|discards|disconsolate|disconsolation|discontented|discord|discounted|discouraged|discredited|disdain|disgrace|disgraced|disguise|disguised|disguises|disguising|disgust|disgusted|disgusting|disheartened|dishonest|disillusioned|disinclined|disjointed|dislike|dismal|dismayed|disorder|disorganized|disoriented|disparage|disparaged|disparages|disparaging|displeased|dispute|disputed|disputes|disputing|disqualified|disquiet|disregard|disregarded|disregarding|disregards|disrespect|disrespected|disruption|disruptions|disruptive|dissatisfied|distort|distorted|distorting|distorts|distract|distracted|distraction|distracts|distress|distressed|distresses|distressing|distrust|distrustful|disturb|disturbed|disturbing|disturbs|dithering|dizzy|dodging|dodgy|does not work|dolorous|dont like|doom|doomed|doubt|doubted|doubtful|doubting|doubts|downcast|downhearted|downside|drag|dragged|drags|drained|dread|dreaded|dreadful|dreading|dreary|droopy|drop|drown|drowned|drowns|drunk|dubious|dud|dull|dumb|dump|dumped|dumps|dupe|duped|dysfunction|eerie|eery|embarrass|embarrassed|embarrasses|embarrassing|embarrassment|embittered|emergency|emptiness|empty|enemies|enemy|ennui|enrage|enraged|enrages|enraging|enslave|enslaved|enslaves|envies|envious|envy|envying|erroneous|error|errors|escape|escapes|escaping|eviction|evil|exaggerate|exaggerated|exaggerates|exaggerating|exclude|excluded|exclusion|excuse|exempt|exhausted|expel|expelled|expelling|expels|exploit|exploited|exploiting|exploits|expose|exposed|exposes|exposing|fad|fail|failed|failing|fails|failure|failures|fainthearted|fake|fakes|faking|fallen|falling|falsified|falsify|farce|fascist|fascists|fatalities|fatality|fatigue|fatigued|fatigues|fatiguing|fear|fearful|fearing|fearsome|fed up|feeble|felonies|felony|fiasco|fidgety|fight|fire|fired|firing|flees|flop|flops|flu|flustered|fool|foolish|fools|forced|foreclosure|foreclosures|forget|forgetful|forgotten|frantic|fraud|frauds|fraudster|fraudsters|fraudulence|fraudulent|frenzy|fright|frightened|frightening|frikin|frowning|frustrate|frustrated|frustrates|frustrating|frustration|fud|fuming|funeral|funerals|furious|gag|gagged|ghost|giddy|gloom|gloomy|glum|goddamn|grave|gray|greed|greedy|green wash|green washing|greenwash|greenwasher|greenwashers|greenwashing|grey|grief|grieved|gross|guilt|guilty|gullibility|gullible|gun|hacked|hapless|haplessness|hard|hardship|harm|harmed|harmful|harming|harms|harried|harsh|harsher|harshest|hate|hated|haters|hates|hating|haunt|haunted|haunts|havoc|heartbreaking|heartbroken|heavyhearted|hell|helpless|hesitant|hesitate|hid|hide|hides|hiding|hindrance|hoax|homesick|hooligan|hooliganism|hooligans|hopeless|hopelessness|horrendous|horrible|horrific|horrified|hostile|huckster|humiliated|humiliation|hunger|hurt|hurting|hurts|hypocritical|hysteria|hysterical|hysterics|idiot|idiotic|ignorance|ignorant|ignore|ignored|ignores|ill|illegal|illiteracy|illness|illnesses|imbecile|immobilized|impatient|imperfect|impose|imposed|imposes|imposing|impotent|imprisoned|inability|inaction|inadequate|incapable|incapacitated|incensed|incompetence|incompetent|inconsiderate|inconvenience|inconvenient|indecisive|indifference|indifferent|indignant|indignation|indoctrinate|indoctrinated|indoctrinates|indoctrinating|ineffective|ineffectively|infected|inferior|inflamed|infringement|infuriate|infuriated|infuriates|infuriating|inhibit|injured|
           injury|injustice|inquisition|insane|insanity|insecure|insensitive|insensitivity|insignificant|insipid|insult|insulted|insulting|insults|interrogated|interrupt|interrupted|interrupting|interruption|interrupts|intimidate|intimidated|intimidates|intimidating|
           intimidation|irate|ironic|irony|irrational|irresolute|irreversible|irritate|irritated|irritating|isolated|itchy|jailed|jealous|jeopardy|joyless|jumpy|kill|killed|killing|kills|lack|lackadaisical|lag|lagged|lagging|lags|lame|lawsuit|lawsuits|lazy|leak|leaked|leave|lethargic|lethargy|liar|liars|libelous|lied|limitation|limited|limits|litigation|litigious|livid|loathe|loathed|loathes|loathing|lobby|lobbying|lonely|lonesome|longing|loom|loomed|looming|looms|loose|looses|loser|losing|loss|lost|lowest|lugubrious|lunatic|lunatics|lurk|lurking|lurks|mad|maddening|made-up|madly|madness|mandatory|manipulated|manipulating|manipulation|meaningless|mediocrity|melancholy|menace|menaced|mess|messed|messing up|mindless|misbehave|misbehaved|misbehaves|misbehaving|mischief|mischiefs|miserable|misery|misgiving|misinformation|misinformed|misinterpreted|misleading|misread|misreporting|misrepresentation|miss|missed|missing|mistake|mistaken|mistakes|mistaking|misunderstand|misunderstanding|misunderstands|misunderstood|moan|moaned|moaning|moans|mock|mocked|mocking|mocks|mongering|monopolize|monopolized|monopolizes|monopolizing|moody|mope|moping|moron|mourn|mourned|mournful|mourning|mourns|mumpish|murder|murderer|murdering|murderous|murders|myth|n00b|naive|nasty|nave|needy|negative|negativity|neglect|neglected|neglecting|neglects|nerves|nervous|nervously|no|no fun|noisy|nonsense|noob|nosey|not good|not working|notorious|numb|nuts|obliterate|obliterated|obnoxious|obscene|obsolete|obstacle|obstacles|obstinate|odd|offend|offended|offender|offending|offends|offline|oppressed|oppressive|optionless|outcry|outmaneuvered|outrage|outraged|overload|overlooked|overreact|overreacted|overreaction|overreacts|oversell|overselling|oversells|oversimplification|oversimplified|oversimplifies|oversimplify|overstatement|overstatements|overweight|oxymoron|pain|pained|panic|panicked|panics|paradox|parley|passive|passively|pathetic|pay|penalty|pensive|peril|perjury|perpetrator|perpetrators|perplexed|persecute|persecuted|persecutes|persecuting|perturbed|pesky|pessimism|pessimistic|petrified|phobic|pileup|pique|piqued|piss|pissed|pissing|piteous|pitied|pity|poised|poison|poisoned|poisons|pollute|polluted|polluter|polluters|pollutes|poor|poorer|poorest|possessive|postpone|postponed|postpones|postponing|poverty|powerless|prblm|prblms|pressure|pressured|pretend|pretending|pretends|prevent|prevented|preventing|prevents|prison|prisoner|prisoners|problem|problems|profiteer|propaganda|prosecute|prosecuted|prosecutes|prosecution|protest|protesters|protesting|protests|provoke|provoked|provokes|provoking|pseudoscience|punish|punished|punishes|punitive|pushy|puzzled|quaking|questionable|questioned|questioning|racism|racist|racists|rage|rageful|rainy|rant|ranter|ranters|rants|rash|rebellion|recession|reckless|refuse|refused|refusing|regret|regretful|regrets|regretted|regretting|reject|rejected|rejecting|rejects|relentless|remorse|repulse|repulsed|resentful|resign|resigned|resigning|resigns|restless|restrict|restricted|restricting|restriction|restricts|retained|retard|retarded|retreat|revenge|revengeful|ridiculous|rig|rigged|riot|riots|risk|risks|rob|robber|robed|robing|robs|ruin|ruined|ruining|ruins|sabotage|sad|sadden|saddened|sadly|sappy|sarcastic|scam|scams|scandal|scandalous|scandals|scapegoat|scapegoats|scare|scared|scary|sceptical|scold|scorn|scornful|scream|screamed|screaming|screams|screwed|sedition|seditious|seduced|self-deluded|selfish|selfishness|sentence|sentenced|sentences|sentencing|severe|shaky|shame|shamed|shameful|shattered|shock|shocked|shocking|shocks|shoot|short-sighted|short-sightedness|shortage|shortages|shy|sick|sigh|silencing|silly|sinful|singleminded|skeptic|skeptical|skepticism|skeptics|slam|slash|slashed|slashes|slashing|slavery|sleeplessness|sluggish|smear|smog|sneaky|snub|snubbed|snubbing|snubs|solemn|somber|sore|sorrow|sorrowful|sorry|spam|spammer|spammers|spamming|speculative|
           spiritless|spiteful|squelched|stab|stabbed|stabs|stall|stalled|stalling|stampede|startled|starve|starved|starves|starving|steal|steals|stereotype|stereotyped|stifled|stingy|stolen|stop|stopped|stopping|stops|strange|strangely|strangled|stressed|stressor|stressors|stricken|strike|strikers|strikes|struck|struggle|struggled|struggles|struggling|stubborn|stuck|stunned|stupid|stupidly|subversive|sucks|suffer|suffering|suffers|suicidal|suicide|suing|sulking|sulky|sullen|suspect|suspected|suspecting|suspects|suspend|suspended|suspicious|swear|swearing|swears|swindle|swindles|swindling|tard|tears|tense|tension|terrible|terribly|terrified|terror|terrorize|terrorized|terrorizes|thorny|thoughtless|threat|threaten|threatened|threatening|threatens|threats|thwart|thwarted|thwarting|thwarts|timid|timorous|tired|tits|toothless|torn|torture|tortured|tortures|torturing|totalitarian|totalitarianism|tout|touted|touting|touts|tragedy|tragic|trap|trapped|trauma|traumatic|travesty|treason|treasonous|trembling|tremulous|tricked|trickery|trouble|troubled|troubles|tumor|ugly|unacceptable|unappreciated|unapproved|unaware|unbelievable|unbelieving|uncertain|unclear|uncomfortable|unconcerned|unconfirmed|unconvinced|uncredited|undecided|underestimate|underestimated|underestimates|underestimating|undermine|undermined|undermines|undermining|undeserving|undesirable|uneasy|unemployment|unequal|unethical|unfair|unfocused|unfulfilled|unhappy|unhealthy|unimpressed|unintelligent|unjust|unlovable|unloved|unmotivated|unprofessional|unresearched|unsatisfied|unsecured|unsettled|unsophisticated|unstable|unsupported|unsure|unwanted|unworthy|upset|upsets|upsetting|uptight|urgent|useless|uselessness|vague|verdict|verdicts|vexation|vexing|vicious|victim|victimize|victimized|victimizes|victimizing|victims|vile|violate|violated|violates|violating|violence|violent|virulent|vitriolic|vociferous|vulnerability|vulnerable|walkout|walkouts|wanker|war|warfare|warn|warned|warning|warnings|warns|waste|wasted|wasting|wavering|weak|weakness|weary|weep|weeping|weird|whitewash|wicked|widowed|withdrawal|woebegone|woeful|worn|worried|worry|worrying|worse|worsen|worsened|worsening|worsens|worst|worthless|wrathful|wreck|wrong|wronged|yucky|zealot|zealots|not|dont",
           test1$Lower, ignore.case = TRUE), "Negative"] =  1

test1[is.na(test1)] = 0 
xg_test = data.frame( dtm_test ,Positive =  test1$Positive , Negative = test1$Negative)

#Xgboost Prediction
pred1 <- predict(xg_train_fit, data.matrix(xg_test))
sub_xgboost = data.frame(actual = test , predict = pred1)

#Naive Bayes Prediction
pred = predict(model, xg_test, type = "raw")
pred = data.frame(pred)
sub_nb = data.frame(actual = test , predict = pred$X1)

#average ensembling of the output is the final step

sub = data.frame(actual = sub_nb$actual.Emotion , Probability = (0.8 * sub_nb$predict + 0.2* sub_xgboost$predict))

Distribution = data.frame( Distribution = sub$Probability)

plot(Distribution$Distribution)

sub$predict = ifelse(sub$Probability > 0.5 , 1,0 )
sub = sub[c(1,3)]

#Confusion Matrix

Confusion_Matrix = as.data.frame(as.matrix(table(sub)))


# Precision & Recall

measurePrecisionRecall <- function(predict, actual){
  precision <- sum(predict & actual) / sum(predict)
  recall <- sum(predict & actual) / sum(actual)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}

Results = measurePrecisionRecall(sub$predict , sub$actual)

