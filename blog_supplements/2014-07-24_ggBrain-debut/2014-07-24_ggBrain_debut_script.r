setwd("/Users/aaronfisher/Documents/JH/Website/pelican/content/blog_supplements/2014-07-24_ggBrain-debut")

library(ggBrain)
library(oro.nifti)

s_map1<-readNIfTI(system.file('seed_corr_1.nii.gz', package='ggBrain'))
s_map2<-readNIfTI(system.file('seed_corr_2.nii.gz', package='ggBrain'))
template <- readNIfTI(system.file('template.nii.gz', package='ggBrain'))
mask <- readNIfTI(system.file('brain_mask.nii.gz', package='ggBrain'))
seed_mask <- readNIfTI(system.file('seed_mask.nii.gz', package='ggBrain'))
nii1_trunc <- readNIfTI(system.file('subj_trunc_1.nii.gz', package='ggBrain'))


library(brainR)

hd_template <- readNIfTI(system.file("MNI152_T1_1mm_brain.nii.gz", package="brainR"))





#####################
# Generate plots
#####################

library(ggplot2)

###############

# seed correlation (type = 'signed')
#png(file='seed_map.png',260,260)

#s_map1 is a seed correlation map
dd<-ggBrain(template=template,brains=s_map1,mask=mask,mar=3,mar_ind=30,type='signed')
dd+labs(title='Seed Correlation Map')+ theme_black_bg()

dev.off()



column_ind = factor(c(1,2,3),labels=c('Sagittal','Coronal','Transverse'))

dd<-ggBrain(brains=seed_mask,template=template,mar=c(1,2,3),
    mar_ind=c(37,18,30),row_ind=c(1,1,1), col_ind=column_ind,
    type='binary',binary_color='black',tri_planar=TRUE,mask=mask)

#png('seed_mask_triplane.png',600,240)
dd + labs(alpha='Seed mask') + theme_black_bg()
dev.off()

dd_1<-ggBrain(brains=s_map1,template=template,mar=mar,mar_ind=mar_ind,row_ind=row_ind,
        col_ind=col_ind,tri_planar=TRUE,mask=mask)+
    scale_alpha(range=c(0,1))+labs(fill='Sign',alpha='Magnitude\n(opacity)')+theme_black_bg()


