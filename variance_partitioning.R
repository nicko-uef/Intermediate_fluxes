library(glmm.hp)
library(MuMIn)

r.squaredGLMM(ch4_mod)
hp_result <- glmm.hp(ch4_mod)
hp_result
plot(hp_result)
