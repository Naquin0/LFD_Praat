# LFD.psc
# Script implemented by Niasche Aquino and Plinio A. Barbosa 
# (IEL/Univ. of Campinas, Brazil) for computing
# local features from coupled audio/TG files 
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; version 2 of the License.
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
# Date: 2022
form File acquisition
 word AudiofileExtension *.wav
 word DataFile data.txt
 integer F0Thresholdleft 75
 integer F0Thresholdright 300
 integer VozTier 1
 integer ContextoTier 2
 integer IdiosTier 3
 integer PalavraTier 4
 positive Smthf0Thr 10
 positive F0step 0.05
 integer Fmax_(Hz) 5000
 integer MaxFormant 5
endform

# Creates the list of all files with the extension given by the variable AudiofileExtension (default = WAV)
Create Strings as file list... list 'AudiofileExtension$'
numberOfFiles = Get number of strings
if !numberOfFiles
  exit There are no sound files in the folder!
endif

# Creates and prints the header of the two output text files given above by the LocalOut variables
filedelete 'DataFile$'
fileappend 'DataFile$' audiofile voice context segment type dur onsetTime melc1mean melc2mean melc3mean melc4mean melc5mean melc6mean melc7mean melc8mean melc9mean melc10mean melc11mean melc12mean melc1sd melc2sd melc3sd melc4sd melc5sd melc6sd melc7sd melc8sd melc9sd melc10sd melc11sd melc12sd f1mean f1med f2mean f2med f3mean f3med f4mean f4med meandf2 CGC sdC skew kurt peakB propeakB peak05 peak1 peak2 peak3 peak4 min05 min1 min2 min3 min4 cg05 cg1 cg2 cg3 cg4 sd05 sd1 sd2 sd3 sd4 mnss rel_int ste 'newline$'

# All computations are done for all files
for ifile from 1 to numberOfFiles
 select Strings list
 audiofile$ = Get string... ifile

# Read each sound file:
 Read from file... 'audiofile$'
 filename$ = selected$("Sound")

# Computes the formant trace:
 To Formant (burg)... 0.0 'maxFormant' 'fmax' 0.025 50

# Reads the corresponding TextGrid file (same name as the Sound file):
 tg$ = filename$ + ".TextGrid"
 Read from file... 'tg$'
 tgname$ = selected$("TextGrid")

# Gets the number of intervals of each Tier:
 nr_intervals_Idios = Get number of intervals... 'idiosTier'
 nr_intervals_Palavra = Get number of intervals... 'palavraTier'
 ##

# Gets the maximum intensity
 select Sound 'filename$'
 To Intensity... f0Thresholdleft 0 1
 max_int = Get maximum... 0 0 none


 select Sound 'filename$'

# Computes the Pitch object for each Sound file and smoothes the F0 contour with tha value informed above by the Smthf0Thr variable.
 To Pitch... 0.0 'f0Thresholdleft' 'f0Thresholdright'
 Smooth... 'smthf0Thr'

# Gets the total duration of the Sound/Pitch file:
 totaldur = Get total duration

# Generates a PitchTier object and interpolate it quadratically for further F0 descriptors computing:
 Down to PitchTier
 Interpolate quadratically... 4.0 Hz
 select TextGrid 'tgname$'



# Computes parameters for VV units
   n = 0
   sum_sq_vvdur = 0
   v_onset# = zero# (nr_intervals_Palavra) 
   v_lab$# = empty$# (nr_intervals_Palavra)
   for j from 1 to nr_intervals_Palavra
     seg$ = Get label of interval... 'palavraTier' j
     firstvowel$ = left$(seg$, 1)
     call isvowel 'firstvowel$'
     if truevowel
        n += 1
        v_onset# [n] = Get start time of interval... 'palavraTier' j
        v_lab$# [n] = seg$ 
     endif  
   endfor

# Computes mean and standard-deviation of durations
   for i from 1 to n - 1
        intVoz = Get interval at time... 'vozTier' v_onset# [i]
        vozLabel$ = Get label of interval... 'vozTier' 'intVoz'
        interval_nr = Get interval at time... 'idiosTier' v_onset# [i]
	context$ = Get label of interval... 'idiosTier' interval_nr
	context$ = replace$(context$," ","", 0)
  	context$ = replace$(context$,"(","", 0)
  	context$ = replace$(context$,")","", 0)
  	context$ = replace$(context$,"?","", 0)
  	context$ = replace$(context$,"/","", 0)
  	context$ = replace$(context$,"\","", 0)
        type$ = "VV"
        seg$ = left$(v_lab$# [i], 1) + "-" + left$(v_lab$# [i + 1], 1)
        itime = v_onset# [i]
        ftime = v_onset# [i + 1]
        segdur = round ((ftime-itime)*1000)
	select Sound 'filename$'
        s_sel = Extract part... itime ftime rectangular 1.0 no
	wlength = number(fixed$((ftime - itime)*0.25,4))
	slength = number(fixed$((ftime - itime)*0.05,4))
        mf_sel = To MFCC... 12 wlength slength 100 100 0 
        mfname$ = selected$("MFCC")
        k = 1
        while k < 14
            mel = To MelSpectrogram... k k 0
      int = To Intensity
            melc_mean'k' = Get mean... 0 0 energy
            melc_sd'k' = Get standard deviation... 0 0
      select MFCC 'mfname$'
          k += 1
    removeObject: mel, int;
        endwhile
  removeObject: s_sel, mf_sel;
	fileappend 'DataFile$' 'filename$' 'vozLabel$' 'context$' 'seg$' 'type$' 'segdur' 'itime:3' 'melc_mean1:3' 'melc_mean2:3' 'melc_mean3:3' 'melc_mean4:3' 'melc_mean5:3' 'melc_mean6:3' 'melc_mean7:3' 'melc_mean8:3' 'melc_mean9:3' 'melc_mean10:3' 'melc_mean11:3' 'melc_mean12:3' 'melc_sd1:3' 'melc_sd2:3' 'melc_sd3:3' 'melc_sd4:3' 'melc_sd5:3' 'melc_sd6:3' 'melc_sd7:3' 'melc_sd8:3' 'melc_sd9:3' 'melc_sd10:3' 'melc_sd11:3' 'melc_sd12:3' --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- --undefined-- 'newline$'
        select TextGrid 'tgname$'
        for k from 1 to 12
            melc_mean'k' = undefined
            melc_sd'k' = undefined
          endif
        endfor
   endfor



# For each interval in the Idios Tier computes several descriptors:
 for j to nr_intervals_Idios

# Simplifies the names given  in the IdioTier when it has symbols like ?, space, parentheses, slashes:
  context$ = Get label of interval... 'idiosTier' j
  context$ = replace$(context$," ","", 0)
  context$ = replace$(context$,"(","", 0)
  context$ = replace$(context$,")","", 0)
  context$ = replace$(context$,"?","", 0)
  context$ = replace$(context$,"/","", 0)
  context$ = replace$(context$,"\","", 0)
  if context$ != ""
    startw = Get start time of interval... 'idiosTier' j
    endw = Get end time of interval... 'idiosTier' j
    meanw = (startw+endw)/2

# Gets label and interval number of the corresponding Voz Tier:
    intVoz = Get interval at time... 'vozTier' 'meanw'
    vozLabel$ = Get label of interval... 'vozTier' 'intVoz'
    select Pitch 'filename$'

# Vowel F1, F2 means in the Palavra Tier:
    select TextGrid 'tgname$'
    stseg = Get high interval at time... 'palavraTier' 'startw'
    endseg = Get low interval at time... 'palavraTier' 'endw'
    mF1 = 0
    mF2 = 0
    mF3 = 0
    mF4 = 0
    medF1 = 0
    medF2 = 0
    medF3 = 0 
    medF4 = 0
    nV = 0

# For each segment in the Palavra Tier computes some measures:
     for l from stseg to endseg
       seg$ = Get label of interval... 'palavraTier' l
       seg$ = replace$(seg$," ","", 0)
      if seg$ != ""
       itime = Get start time of interval... 'palavraTier' l
       ftime = Get end time of interval... 'palavraTier' l
       mtime = (itime+ftime)/2
       firstvowel$ = left$(seg$, 1)
       call isvowel 'firstvowel$'
       type$ = "C"
       f1mean = undefined
       f1med = undefined
       f2mean = undefined
       f2med = undefined
       meandf2 = undefined
       f3mean = undefined
       f3med = undefined
       f4mean = undefined
       f4med = undefined
       meanste = undefined
       i=1
       while i < 6
        cg'i' = undefined
        sd'i' = undefined
        peak'i' = undefined
        min'i' = undefined
        i += 1
       endwhile
       if truevowel
        type$ ="V"
        select Formant 'filename$'
        f1 = Get value at time... 1 'mtime' Hertz Linear
        f2 = Get value at time... 2 'mtime' Hertz Linear
        f3 = Get value at time... 3 'mtime' Hertz Linear
        f4 = Get value at time... 4 'mtime' Hertz Linear
        f1med = Get quantile... 1 'itime' 'ftime' Hertz 0.5
        f2med = Get quantile... 2 'itime' 'ftime' Hertz 0.5
        f3med = Get quantile... 3 'itime' 'ftime' Hertz 0.5
        f4med = Get quantile... 4 'itime' 'ftime' Hertz 0.5
        mF1 = mF1 + f1
        mF2 = mF2 + f2
        mF3 = mF3 + f3
        mF4 = mF4 + f4
        medF1 = f1med + medF1
        medF2 = f2med + medF2
        medF3 = f3med + medF3
        medF4 = f4med + medF4
        nV = nV + 1

# Computes f2 discrete derivative, and its cumulative value
        f2right = ftime
        meandf2 = 0 
        f2ant = Get value at time... 2 'itime' Hertz Linear
        i = 1
        timef2 = f0step+itime
        while timef2 <= f2right
           f2current = Get value at time... 2 'timef2' Hertz Linear
           df2'i' = f2current - f2ant
           meandf2 = meandf2 + df2'i'
           f2ant = f2current
           timef2 = timef2 + f0step
           i=i+1
        endwhile
        i = i -1
        meandf2= meandf2/i

# Computes formants mean and median:
       f1mean = mF1/nV
       f2mean = mF2/nV
       f3mean = mF3/nV
       f4mean = mF4/nV
       f1med = medF1/nV
       f2med = medF2/nV
       f3med = medF3/nV
       f4med = medF4/nV

# Computes short-term energy
       frame = itime
       step = 0.010
       ste = 0
       n = 0
       while frame+step < ftime 
          select Sound 'filename$'
          s_part = Extract part... frame frame+step Hanning 1.0 no
          ste += Get energy... 0 0
          frame += step*0.5
          n+=1
          removeObject: s_part;
       endwhile
       meanste = 1000*(ste/n)
       endif

# Computes the duration of each segment:
       segdur = round(1000*(ftime-itime))

# Verifies if the segment is a fricative, lateral or nasal:
      call isfriclatnasplo 'seg$'
      if friclatnasplo == 1
        type$ = "F"
      elif friclatnasplo == 2
        type$ = "L"
      elif friclatnasplo == 3
        type$ = "N"
      else
        if friclatnasplo == 4
          type$ = "P" 
        endif
      endif

# Computes the centre of gravity mean and standard-deviation if the segment is a vowel, fricative, lateral or nasal: 
     cgC = undefined
     sdC = undefined
     skew = undefined
     kurt = undefined
     if truevowel or type$ == "F" 
       select Sound 'filename$'
       s_part=Extract part... itime ftime rectangular 1.0 no
       sp_part=To Spectrum... yes
       cgC = Get centre of gravity... 2.0
       sdC = Get standard deviation... 2.0
       skew = Get skewness... 2.0
       kurt = Get kurtosis... 2.0
       removeObject: sp_part, s_part;
     endif

# Computes fricatives' parameters
   mnss = undefined
   rel_int = undefined
   dist_prv = undefined
   dist_nxt = undefined
   if type$ == "F"
      select Sound 'filename$'
      s_part = Extract part... itime ftime rectangular 1.0 no
      f_int = Get intensity (dB)
      removeObject: s_part;
      
      select TextGrid 'tgname$'
      st_tier = Get interval at time... 'palavraTier' mtime
      i = 1
      k = 1
      while i <> 0 and k <> 0
        if st_tier + i < nr_intervals_Palavra
          segV$ = Get label of interval... 'palavraTier' st_tier+i
          firstvowel$ = left$(segV$, 1)
          call isvowel 'firstvowel$'
          if truevowel
            itime_nxt = Get start time of interval... 'palavraTier' st_tier+i
            ftime_nxt = Get end time of interval... 'palavraTier' st_tier+i
            dist_nxt = (ftime_nxt + itime_nxt)/2 - mtime
            i = 0
          else
            i += 1
          endif
        else
            i = 0
        endif
        if st_tier - k > 0
          select TextGrid 'tgname$'
          segV$ = Get label of interval... 'palavraTier' st_tier-k
          firstvowel$ = left$(segV$, 1)
          call isvowel 'firstvowel$'
          if truevowel
            itime_prv = Get start time of interval... 'palavraTier' st_tier-k
            ftime_prv = Get end time of interval... 'palavraTier' st_tier-k
            dist_prv = mtime - (ftime_prv + itime_prv)/2 
            k = 0
          else
            k += 1
          endif
        else
          k = 0
        endif
      endwhile
      if dist_prv - dist_nxt > 0
        select Sound 'filename$'
        s_part=Extract part... itime_nxt ftime_nxt rectangular 1.0 no
        v_int_nxt = Get intensity (dB)
        rel_int = f_int/v_int_nxt
  removeObject: s_part;
      else
        select Sound 'filename$'
        s_part = Extract part... itime_prv ftime_prv rectangular 1.0 no
        v_int_prv = Get intensity (dB)
        rel_int = f_int/v_int_prv
  removeObject: s_part;
      endif
   endif

# Computes plosives' parameters
  peakB = undefined
  propeakB = undefined
  if type$ == "P"
    select Sound 'filename$'
    s_part = Extract part... itime ftime rectangular 1.0 no
    sp_part = To Spectrum... yes
    spectrumseg$ = selected$("Spectrum")
    select Spectrum 'spectrumseg$'
    pw_part = To PowerCepstrum
    #Frequência do Burst 
    peakB = Get peak... 'F0Thresholdleft' 'F0Thresholdright' Parabolic
    #Proeminência do Burst
    propeakB = Get peak prominence... 'F0Thresholdleft' 'F0Thresholdright' "Parabolic" 0.001 0 "Straight" Robust
    removeObject: s_part, sp_part, pw_part;
  endif

# Computes laterals' parameters
   if type$ == "L"
    select Sound 'filename$'
    s_part = Extract part... itime ftime rectangular 1.0 no
    sp_part = To Spectrum... yes
    spectrumseg$ = selected$("Spectrum")
    i = 1
    removeObject: s_part, sp_part;
     while i < 6
      select Sound 'filename$'
      s_part = Extract part... itime ftime rectangular 1.0 no
      sp_part = To Spectrum... yes
      spectrumseg$ = selected$("Spectrum")
      select Spectrum 'spectrumseg$'
      if i == 1
        Filter (pass Hann band): 0, 500 , 50
        cg1 = Get centre of gravity... 2.0
        sd1 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak1 = Get frequency of maximum... 100 500 parabolic
        min1 = Get frequency of minimum... 100 500 parabolic
  removeObject: lt_part;
      elsif i == 2
  Filter (pass Hann band): 500, i * 500 , 50
        cg2 = Get centre of gravity... 2.0
        sd2 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak2 = Get frequency of maximum... 500 i*500 parabolic
        min2 = Get frequency of minimum... 500 i*500 parabolic
  removeObject: lt_part;
      elsif i ==3
        Filter (pass Hann band): 1000, 2000, 50
        cg3 = Get centre of gravity... 2.0
        sd3 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak3 = Get frequency of maximum... 1000 2000  parabolic
        min3 = Get frequency of minimum... 1000 2000 parabolic
  removeObject: lt_part;
      elsif i ==4
        Filter (pass Hann band): 2000, 3000, 50
        cg4 = Get centre of gravity... 2.0
        sd4 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak4 = Get frequency of maximum... 2000 3000 parabolic
        min4 = Get frequency of minimum... 2000 3000 parabolic
  removeObject: lt_part;
       else
        Filter (pass Hann band): 3000, 4000, 50
        cg5 = Get centre of gravity... 2.0
        sd5 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak5 = Get frequency of maximum... 3000 4000 parabolic
        min5 = Get frequency of minimum... 3000 4000 parabolic
  removeObject: lt_part;
      endif
      removeObject: s_part, sp_part;
      i += 1
    endwhile
  endif

# Computes nasals' parameters
  if type$ == "N"
    select Sound 'filename$'
    s_part = Extract part... itime ftime rectangular 1.0 no
    sp_part = To Spectrum... yes
    spectrumseg$ = selected$("Spectrum")
    i = 1
          removeObject: s_part, sp_part;
while i < 6
      select Sound 'filename$'
      s_part = Extract part... itime ftime rectangular 1.0 no
      sp_part = To Spectrum... yes
      spectrumseg$ = selected$("Spectrum")
      select Spectrum 'spectrumseg$'
      if i == 1
        Filter (pass Hann band): 0, 500 , 50
        cg1 = Get centre of gravity... 2.0
        sd1 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak1 = Get frequency of maximum... 100 500 parabolic
        min1 = Get frequency of minimum... 100 500 parabolic
  removeObject: lt_part;
      elsif i == 2
  Filter (pass Hann band): 500, i * 500 , 50
        cg2 = Get centre of gravity... 2.0
        sd2 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak2 = Get frequency of maximum... 500 i*500 parabolic
        min2 = Get frequency of minimum... 500 i*500 parabolic
  removeObject: lt_part;
      elsif i ==3
        Filter (pass Hann band): 1000, 2000, 50
        cg3 = Get centre of gravity... 2.0
        sd3 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak3 = Get frequency of maximum... 1000 2000  parabolic
        min3 = Get frequency of minimum... 1000 2000 parabolic
  removeObject: lt_part;
      elsif i ==4
        Filter (pass Hann band): 2000, 3000, 50
        cg4 = Get centre of gravity... 2.0
        sd4 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak4 = Get frequency of maximum... 2000 3000 parabolic
        min4 = Get frequency of minimum... 2000 3000 parabolic
  removeObject: lt_part;
       else
        Filter (pass Hann band): 3000, 4000, 50
        cg5 = Get centre of gravity... 2.0
        sd5 = Get standard deviation... 2.0
        lt_part = To Ltas (1-to-1)
        peak5 = Get frequency of maximum... 3000 4000 parabolic
        min5 = Get frequency of minimum... 3000 4000 parabolic
  removeObject: lt_part;
      endif
      removeObject: s_part, sp_part;
      i += 1
    endwhile
  endif

# Writes down the local parameters computed above:
       fileappend 'DataFile$' 'filename$' 'vozLabel$' 'context$' 'seg$' 'type$' 'segdur' 'itime:3' 'melc_mean1:3' 'melc_mean2:3' 'melc_mean3:3' 'melc_mean4:3' 'melc_mean5:3' 'melc_mean6:3' 'melc_mean7:3' 'melc_mean8:3' 'melc_mean9:3' 'melc_mean10:3' 'melc_mean11:3' 'melc_mean12:3' 'melc_sd1:3' 'melc_sd2:3' 'melc_sd3:3' 'melc_sd4:3' 'melc_sd5:3' 'melc_sd6:3' 'melc_sd7:3' 'melc_sd8:3' 'melc_sd9:3' 'melc_sd10:3' 'melc_sd11:3' 'melc_sd12:3' 'f1mean:3' 'f1med:3' 'f2mean:3' 'f2med:3' 'f3mean:3' 'f3med:3' 'f4mean:3' 'f4med:3' 'meandf2:3' 'cgC:0' 'sdC:0' 'kurt:3' 'skew:3' 'peakB:3' 'propeakB:3' 'peak1:3' 'peak2:3' 'peak3:3' 'peak4:3' 'peak5:3' 'min1:3' 'min2:3' 'min3:3' 'min4:3' 'min5:3' 'cg1:3' 'cg2:3' 'cg3:3' 'cg4:3' 'cg5:3' 'sd1:3' 'sd2:3' 'sd3:3' 'sd4:3' 'sd5:3' 'mnss:3' 'rel_int:3' 'meanste:3' 'newline$'
       select TextGrid 'tgname$'
     endif
     endfor
  endif
  select TextGrid 'tgname$'
 endfor
endfor

select all
Remove

# Phonological classifier of the segment
procedure isvowel temp$
 truevowel = 0
 if temp$ = "i" or  temp$ = "e"  or temp$ = "a"  or temp$ = "o"  or temp$ = "u" or temp$ = "I" or temp$ = "E"
    ...or temp$ = "A"  or temp$ = "y" or temp$ = "O"  or temp$ = "U" or temp$ = "6"  or temp$ = "@"
    ...or temp$ = "2" or temp$ = "9" or temp$ = "Y" 
    truevowel = 1
 endif
endproc 

procedure isfriclatnasplo temp$
 if temp$ = "s" or  temp$ = "S" or temp$ = "sh"  or temp$ = "f"  or temp$ = "v" or  temp$ = "z"  or temp$ = "Z" or temp$ = "zh" 
     friclatnasplo = 1
 elif temp$ = "l" or temp$ = "lh" 
  friclatnasplo = 2
 elif temp$ = "m" or temp$ = "n" or temp$ = "nh" 
    friclatnasplo = 3
 elif temp$ = "p" or temp$ = "t" or temp$ = "k" or temp$ = "b" or temp$ = "d" or temp$ = "g"
  friclatnasplo = 4
 else
    friclatnasplo = 0
 endif
endproc