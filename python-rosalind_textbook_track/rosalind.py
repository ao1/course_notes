# Question List: http://rosalind.info/problems/list-view/?location=bioinformatics-textbook-track

import os
import itertools

def txtfile_to_string(default_path,input_filename):

    with open (os.path.join(default_path,input_filename), "r") as myfile:
        data=myfile.read().replace('\n', '')

    return data

def list_to_txtfile(output_filename,output_list,with_newlines=False):

    if with_newlines:
        with open(output_filename, "w") as f:
            f.write('\n'.join(map(str,output_list)))
        print("Output to: {0}".format(os.path.join(os.getcwd(),output_filename)))
    else:
        with open(output_filename, "w") as f:
            f.write(' '.join(map(str,output_list)))
        print("Output to: {0}".format(os.path.join(os.getcwd(),output_filename)))

def most_frequent_kmers_of_length(dnastring,length):

    kmerfreq = {}
    max_results = []

    #iterate over dnastring and keep kmer frequencies in dictionary kmerfreq
    for i, letter in enumerate(dnastring):
        if len(dnastring[i:i+length]) == length:
            if dnastring[i:i+length] in kmerfreq:
                kmerfreq[dnastring[i:i+length]] += 1
            else:
                kmerfreq[dnastring[i:i+length]] = 1

    #put highest occuring kmers in a list
    max_occurences = max(kmerfreq.values())

    for key,value in kmerfreq.items():
        if value == max_occurences:
            max_results.append(key)

    #print and return results as a list
    print(*max_results,sep=' ')
    return max_results

def reverse_complement(dnastring):

    #make translation table
    trans_in = 'ACTG'
    trans_out = 'TGAC'
    trans_table = str.maketrans(trans_in,trans_out)

    #reverse dnastring
    dnastring = dnastring[::-1]

    #print and return translated string
    print(dnastring.translate(trans_table))
    return dnastring.translate(trans_table)

def return_indexes(genome,pattern):

    index_list = []

    #iterate over genome and add all occurence indexes to an index list
    for i, letter in enumerate(genome):
        if genome[i:i+len(pattern)] == pattern:
            index_list.append(i)

    #print and return index results as a list
    print(*index_list,sep=' ')
    return index_list


def find_clumps(genome,kmer_length,interval_length,minimum_frequency):

    number_of_intervals = 0
    number_of_kmers = 0
    qualifying_kmers = set()

    #for each kmer in each interval of length interval_length
    for i, letter in enumerate(genome):
        interval = genome[i:i+interval_length]
        if len(interval) == interval_length:
            number_of_intervals += 1
            kmerlist = {}
            #populate a new dictionary with frequency of kmers with length kmer_length
            for j, letter in enumerate(interval):
                kmer = interval[j:j+kmer_length]
                if len(kmer) == kmer_length:
                    number_of_kmers += 1
                    if kmer in kmerlist:
                        kmerlist[kmer] += 1
                    else:
                        kmerlist[kmer] = 1
            #add kmers occuring minimum_frequency times or more to the qualifying_kmers set
            for key, value in kmerlist.items():
                if value >= minimum_frequency:
                    qualifying_kmers.add(key)

    #print and return qualifying_kmers set, return it as a list
    print(*qualifying_kmers,sep=' ')
    return list(qualifying_kmers)

def minimum_skew(genome):

    counter = 0
    skew_list = []
    min_indexes = []

    for letter in genome:
        if letter == "G": counter +=1
        if letter == "C": counter -=1
        skew_list.append(counter)

    skew_list.insert(0,0)
    minskew = min(skew_list)

    for i, value in enumerate(skew_list):
        if value == minskew:
            min_indexes.append(i)

    print(*min_indexes,sep=' ')
    return min_indexes

def string_distance(string1,string2):

    assert len(string1) == len(string2)
    number_of_differences = 0
    for i, value in enumerate(string1):
        if string1[i] != string2[i]:
            number_of_differences += 1

    return number_of_differences

def approximate_pattern_match(pattern,genome,max_mismatch):

    qualifying_indexes = []

   #iterate over genome in pattern-length chunks,
   #if string difference less than or equal to max_mismatch, add index of chunk to qualifying_indexes list
    for i, letter in enumerate(genome):
        if len(genome[i:i+len(pattern)]) == len(pattern):
            if string_distance(genome[i:i+len(pattern)],pattern) <= max_mismatch:
                qualifying_indexes.append(i)

    #print and return qualifying_indexes list
    print(*qualifying_indexes, sep=" ")
    return qualifying_indexes

def most_frequent_kmers_with_mismatch(genome,kmer_length,mismatch):

    candidate_kmers = set()
    kmerfreq = {}
    max_results = []

    #iterate over genome in kmer_length chunks, generate possibilities, keep ones with appropriate mismatch
    for i,letter in enumerate(genome):
        kmer = genome[i:i+kmer_length]
        if len(kmer) == kmer_length:
            for variant in itertools.product(kmer, repeat=kmer_length):
                if string_distance(kmer,variant) <= mismatch:
                    candidate_kmers.add("".join(variant))

    #count occurences of all candidate kmers in kmerfreq
    for i, letter in enumerate(genome):
        for candiate in candidate_kmers:
            kmer = genome[i:i+kmer_length]
            if len(kmer) == len(candiate) and string_distance(kmer,candiate) <= mismatch:
                if candiate in kmerfreq:
                    kmerfreq[candiate] += 1
                else:
                    kmerfreq[candiate] = 1

    #put highest occuring kmers in a list
    max_occurences = max(kmerfreq.values())

    for key,value in kmerfreq.items():
        if value == max_occurences:
            max_results.append(key)

    #print and return results as a list
    print(*max_results,sep=' ')
    return max_results


def compute_frequency_array(dnastring,kmer_length):

    kmerfreq = [[0 for i in range(4**kmer_length)] for j in range(2)]

    #fill first row of 2d list with combination names
    for i,combination in enumerate(itertools.product("ACGT",repeat=kmer_length)):
        kmerfreq[0][i] = "".join(combination)

    #fill second row of 2d list with occurence frequency in dnastring
    for i,combination in enumerate(kmerfreq[0]):
        for j,letter in enumerate(dnastring):
            kmer = dnastring[j:j+kmer_length]
            if combination == kmer and len(kmer) == kmer_length:
                kmerfreq[1][i] += 1

    #return the second row of 2d list kmerfreq and write it to frequency_array.txt as well
    list_to_txtfile("frequency_array.txt",kmerfreq[1])
    return kmerfreq[1]

def generate_neighbors(dnastring,mismatch_limit):

    qualifying_neighbors = []

    #generate possibilities, keep ones with less than or equal mismatch
    for variant in itertools.product("ACGT", repeat=len(dnastring)):
        if string_distance(dnastring,variant) <= mismatch_limit:
            qualifying_neighbors.append("".join(variant))

    #print and return results as a list
    list_to_txtfile("neighbors.txt",qualifying_neighbors,with_newlines=True)
    return qualifying_neighbors


#===============================================================

default_path = ''
filename = ''
data = txtfile_to_string(default_path,filename)

#sample quick data inputs:

#most_frequent_kmers_of_length("ACTGATCAGC",4)
#reverse_complement("AAAACCCGGT")
#return_indexes("GATATATGCATATACTT","ATAT")
#find_clumps("CGGACTCGACAGATGTGAAGAAATGTGAAGACTGAGTGAAGAGAAGAGGAAACACGACACGACATTGCGACATAATGTACGAATGTAATGTGCCTATGGC",5,74,4)
#minimum_skew("CCTATCGGTGGATTAGCATGTCCCTGTACGTTTCGCCGCGAACTAGTTCACACGGCTTGATGGCAAATGGTTTTTCCGGCGACCGTAATCGTCCACCGAG")
#string_distance("ABC","ABD")
#approximate_pattern_match("ATTCTGGA","CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAATGCCTAGCGGCTTGTGGTTTCTCCTACGCTCC",3)
#generate_nearby_kmers("GATCAT")
#most_frequent_kmers_with_mismatch("ACGTTGCATGTCGCATGATGCATGAGAGCT",4,1)
#most_frequent_kmers_with_mismatch_and_reverse("ACGTTGCATGTCGCATGATGCATGAGAGCT",4,1)
#compute_frequency_array("ACGCGGCTCTGAAA",2)
#generate_neighbors("ACG",1)