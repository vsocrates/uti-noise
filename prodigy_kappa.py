import jsonlines
from collections import defaultdict, Counter
import numpy as np


def has_overlap(tuples, search):
    overlap = False
    for t in tuples:
        if(t['end']>search['start'] and t['start']<search['end']):
            overlap = True
            break
    return overlap

def compute_prodigy_ner_F1(labels, gold_file, ann_file, overlap="strict", weighted=False):
    '''Takes in a list of labels and 
       a set of prodigy jsonl annotation files and 
       computes the F1 measure between two annotations. 
       
       Currently support two overlap options: strict, and type described here: 
       https://www.davidsbatista.net/blog/2018/05/09/Named_Entity_Evaluation/
       
       See: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1090460/
       
       Skips docs that don't have annotations for both annotators. 
    '''
    anns = []
    gold_anns = []

    # read in both annotation files
    with jsonlines.open(gold_file) as reader:
        for ann in reader:
            gold_anns.append(ann)

    with jsonlines.open(ann_file) as reader:
        for ann in reader:
            anns.append(ann)

    # make sure each doc has a spans item, since it isn't added if no spans are annotated
    # TODO: check this is right
    for ann in gold_anns:
        ann.pop('tokens')
        if "versions" in ann.keys():
            ann.pop('versions')
        if "spans" not in ann.keys():
            ann['spans'] = []

    for ann in anns:
        ann.pop('tokens')
        if "versions" in ann.keys():
            ann.pop('versions')    
        if "spans" not in ann.keys():
            ann['spans'] = []

    # combine all text by input hash into a dictionary
    anns_by_input_hash = defaultdict(dict)
    for ann in gold_anns:
        anns_by_input_hash[ann['_input_hash']]["gold"] = ann
    for ann in anns:
        anns_by_input_hash[ann['_input_hash']]["compare"] = ann

    # make sure we only have 5 values in our dictionary
    gold_spans = [ann['spans'] for ann in gold_anns]
    gold_spans = [item for sublist in gold_spans for item in sublist]
    gold_span_support = Counter([span['label'] for span in gold_spans])
    
    anns_spans = [ann['spans'] for ann in anns]
    anns_spans = [item for sublist in anns_spans for item in sublist]

    spans = gold_spans + anns_spans

    # get all the keys in the key-value pairs of the span dicts
    span_keys = [list(span.keys()) for span in spans]
    span_keys = [item for sublist in span_keys for item in sublist]
    span_keys = set(span_keys)

    needed_keys = set(['start', 'end', 'token_start', 'token_end', 'label'])
    extra_keys = list(span_keys - needed_keys)

    # drop them from the dict of spans by hash
    for input_hash, anns in anns_by_input_hash.items():
        for span in anns['gold']['spans']:
            for key in extra_keys:
                if key in span:
                    span.pop(key)        
        for span in anns['compare']['spans']:
            for key in extra_keys:
                if key in span:
                    span.pop(key)

    # all of the above was preprocessing of the prodigy annotation file


    f1_by_label = {}
    for label in labels:
        conf_mat = np.zeros(shape=(2,2))
        for input_hash, anns in anns_by_input_hash.items():
            # make sure each annotation has both annotators
            if anns['gold'] and anns['compare']:
                label_cmp_spans = [span for span in anns['compare']['spans'] if span['label'] == label]
                label_gold_spans = [span for span in anns['gold']['spans'] if span['label'] == label]

                if overlap == "strict":
                    # this requires exact match of surface span and entity type
                    label_cmp_spans.sort(key=lambda x: x['start'], reverse=False)
                    label_gold_spans.sort(key=lambda x: x['start'], reverse=False)
                    set_cmp = set(tuple(sorted(d.items())) for d in label_cmp_spans)
                    set_gold = set(tuple(sorted(d.items())) for d in label_gold_spans)

                    conf_mat[0,0] += len(set_gold.intersection(set_cmp))
                    # we don't know this value because we don't know the total number of possible phrases
                    # to be annotated in NER
                    # conf_mat[1,1] = set_gold.set_difference(set_cmp)
                    conf_mat[0,1] += len(set_cmp - set_gold)
                    conf_mat[1,0] += len(set_gold -  set_cmp)

                elif overlap == "type":
                    # this requres partial match of surface span and match of entity type
                    label_cmp_spans.sort(key=lambda x: x['start'], reverse=False)
                    label_gold_spans.sort(key=lambda x: x['start'], reverse=False)

                    # first we loop through all the trainer annotations and check matches in the gold
                    for cmp_span in label_cmp_spans:
                        if has_overlap(label_gold_spans, cmp_span):
                            conf_mat[0,0] += 1
                        else:
                            conf_mat[0,1] +=1
                    
                    # then we make sure there aren't any the gold that were missed by the trainer annotator
                    for gold_span in label_gold_spans:
                        if has_overlap(label_cmp_spans, gold_span):
                            # we avoid double counting the TPs
                            continue
                        else:
                            conf_mat[1,0] +=1


        a = conf_mat[0,0]
        b = conf_mat[0,1]
        c = conf_mat[1,0]

        if (a + b + c) == 0:
            f1_by_label[label] = 0
        else:
            f1_by_label[label] = 2*a/(2*a + b + c)
    
    if weighted:
        return f1_by_label, gold_span_support
    else:
        return f1_by_label