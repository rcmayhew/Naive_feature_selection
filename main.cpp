#include <iostream>
#include <memory>
#include <vector>
#include <fstream>
#include <sstream>
#include <math.h>
#include <limits>
#include <ctime>

using namespace std;

//6,9,13 - big
//7,10 - litte
void print(const vector<vector<shared_ptr<double>>> & features) {
    for (size_t i = 0; i < features.size(); ++i) {
        for (size_t j = 0; j < features.at(i).size(); ++j) {
            cout << *features.at(i).at(j) << " ";
        }
        cout << endl;
    }
}

string ListToString(vector<bool> list) {
    stringstream tran;
    bool first = false;
    for(size_t i = 0; i < list.size(); ++i) {
        if(list.at(i)) {
            if(!first) {
                first = true;
                 tran << (i + 1);
            }
            else {
                tran << ',';
                tran << (i + 1);
            }
        }
    }
    return tran.str();
    
}

void Normalize(vector<vector<shared_ptr<double>>> & base) {
    size_t n = base.size(), feats = base.at(0).size() - 1;
    double mean, std;
    for (size_t i = 1; i < feats; ++i) {
        mean = 0;
        std = 0;
        for (size_t j = 0; j < n; ++j) {
           mean += *base.at(j).at(i);
        }
        mean /= n;
        for (size_t k = 0; k < n; ++k) {
            std += pow((*base.at(k).at(i) - mean),2);
        }
        std /= n;
        std = sqrt(std);
        for (size_t h = 0; h < n; ++h) {
            *base.at(h).at(i) = (*base.at(h).at(i) - mean)/std;
        }
    }
}


//reads in the in file and return the set of all features
vector<vector<shared_ptr<double>>> & ReadIn(ifstream & file, 
                    vector<vector<shared_ptr<double>>> & base) {
    base.clear();
    double b;
    string hold;

    if(!file.is_open()) exit(1);
    while (!(file.eof())) {
    	getline(file, hold);
    	stringstream tran(hold);
    	vector<shared_ptr<double>> line;
    	while(tran >> b) {
    		shared_ptr<double> a = make_shared <double> (b);
    		line.push_back(a);
    	}
    	if(line.size() != 0) base.push_back(line);
    }
    file.close();
    return base;
    
}

double Distance(const vector<shared_ptr<double>> & start, const vector<shared_ptr<double>> & end, const vector<bool> list) {
    double distance = 0.0000;
    for (size_t i = 1; i < start.size(); ++i) {
        if(list.at(i - 1)) distance += pow( (*start.at(i) - *end.at(i)), 2);
    }
    return sqrt(fabs(distance));
    
}

bool Nearest_Neighbor(const vector<vector<shared_ptr<double>>> & features, const vector<shared_ptr<double>> & data, size_t index, const vector<bool> & list) {
    double lowest_distance = numeric_limits<double>::max(), current_distance;
    size_t closest;
    for (size_t i = 0; i < features.size(); ++i) {
        if(i != index) {
            current_distance = Distance(data, features.at(i), list);
            if(current_distance < lowest_distance) {
                closest = i;
                lowest_distance = current_distance;
            }
        }
    }
    if(*data.at(0) == *features.at(closest).at(0)) return true;
    return false;
}

double leave_one_out(const vector<vector<shared_ptr<double>>> & features, const vector<bool> & list) {
	double times_right = 0;
	for(size_t i = 0; i < features.size(); ++i) {
	    if(Nearest_Neighbor(features, features.at(i), i, list)) times_right++;
	}
	//cout << times_right << endl;
	return 100 * times_right / features.size();
	
}


vector <bool> Backward_Elimination(const vector<vector<shared_ptr<double>>> & features) {
	size_t card = features.at(0).size() - 1;
	vector <bool> list(card,true), best(card, true), use(card, true);
	double best_so_far = 0, accuracy, best_right_now = 0;
	int feature_to_be_removed;
	accuracy = leave_one_out(features, use);
    
    // cout << "Running nearest neighbor with all " << card <<  " features, using "
    // << "“leaving-one-out” evaluation, I get an accuracy of " << accuracy << "%" <<
    // endl << endl << "Beginning search" << endl << endl;
	for(size_t i = 0; i < (card - 1); ++i) {
		best_so_far = 0;
	
		for(size_t j = 0; j < card; ++j) {
		    use = list;
			if(list.at(j)){
			    use.at(j) = false;
				accuracy = leave_one_out(features, use);
				// cout << "\t Using feature(s) {" << ListToString(use) 
				// << "} accuracy is " << accuracy << "%" << endl;
				if(accuracy >= best_so_far) {
					best_so_far = accuracy;
					feature_to_be_removed = j;
				}
			}

		}
		
		if(best_right_now >= best_so_far) {
			list.at(feature_to_be_removed) = false;
		    cout << endl << " {" << ListToString(list) << "}, " 
            << best_so_far << "%" << endl << endl;

		}
		else {
		    best_right_now = best_so_far;
		    list.at(feature_to_be_removed) = false;
		    cout << endl << " {" << ListToString(list) 
		    << "}, " << best_right_now << "%" << endl << endl;
		    best = list;
		}
		
		
	}
	cout << "Finished search!! The best feature subset is {" << ListToString(best)
	<< "}, which has an accuracy of " << best_right_now << "%" << endl;
	return list;

}

vector <bool> Forward_Selection(const vector<vector<shared_ptr<double>>> & features, 
								double & constraint, size_t repetitions) {
	size_t card = features.at(0).size() - 1;
	vector <bool> list(card,false), best(card, false), use(card,true);
	double best_so_far = 0, accuracy, best_right_now = 0;
	int feature_to_be_added = 0;
	accuracy = leave_one_out(features,use);
    
    cout << "Running nearest neighbor with all " << card <<  " features, using "
    << "'leaving-one-out' evaluation, I get an accuracy of " << accuracy << "%" <<
    endl << endl << "Beginning search" << endl << endl;
	for(size_t i = 0; i < card && i < repetitions; ++i) {
		best_so_far = 0;
		for(size_t j = 0; j < card; ++j) {
	        use = list;
			if(!list.at(j)){
			    use.at(j) = true;    
		        //cout << best_right_now << " " << best_so_far;
				accuracy = leave_one_out(features,use);
				// cout << "\t Using feature(s) {" << (j + 1);
    //             if(i != 0) cout << ',';
				// cout << ListToString(list) 
				// << "} accuracy is " << accuracy << "%" << endl;
				if(accuracy > best_so_far) {
					best_so_far = accuracy;
					feature_to_be_added = j;
				}
				if(accuracy > constraint) {
					list.at(feature_to_be_added) = true;
				// 	cout << "Found a set that is higher than constraint. Best set is {" 
				// 	<< ListToString(list) << "} was best, accuracy is " << best_so_far 
				// 	<< "%" << endl << endl;
					constraint = accuracy;
					return list;
				}
			}

		}
		
		if(best_right_now > best_so_far) {
			list.at(feature_to_be_added) = true;
		    cout << endl << " {" << ListToString(list) << "}, " 
            << best_so_far << "%" << endl << endl;

		}
		else {
		    best_right_now = best_so_far;
		    list.at(feature_to_be_added) = true;
		    cout << endl << " {" << ListToString(list) 
		    << "} " << best_right_now << "%" << endl << endl;
		    best = list;
		}
		
		
	}
	cout << "Finished search!! The best feature subset is {" << ListToString(best)
	<< "}, which has an accuracy of " << best_right_now << "%" << endl;
	return list;

}

size_t N_Effect(size_t n, size_t reps) {
	//if rep is 1 then n is n0 + (n - n0)/3.33333
	//if rep is 2 then n is n1 + (n - n1)/3.3333
	//if rep is 2 then n is n2 + (n - n2)/3.3333
	
	size_t n0 = 0;
	for (size_t i = 0; i < reps; ++i) {
		n0 += ((n - n0)/3);
	}
	return n0;
}

vector <bool> Iterative_Deepening(const vector<vector<shared_ptr<double>>> & features,
                                double constraint) {
    size_t n0 = features.at(0).size(), reps = 1, n = N_Effect(n0,reps);
    double holding = constraint; 
    vector<bool> list = Forward_Selection(features, constraint, n);
    while (constraint < holding) {
    	cout << "Did not find a set that matched our constraint, searching deeper" << endl << endl;
    	++reps;
    	n = N_Effect(n0,reps);
    	constraint = holding;
    	list = Forward_Selection(features, constraint, n);

    }

    //we found a match
    cout << "Found a set that is higher than constraint. Best set is {" 
	<< ListToString(list) << "} was best, accuracy is " << constraint 
	<< "%" << endl << endl;

    
    return list;
}

int main( int argc, char *argv[] ) {
    
    srand(time(NULL));
    string type, hold;
    int alg;
    if(argc > 1) {
        type = argv[1];
		alg = 1;
    }
    else {
        cout <<"Welcome to Robert Mayhew Feature Selection Algorithm." << endl <<
        "Type in the name of the file to test :   ";
        getline(cin,type);
        cout << "Type the number of the algorithm you want to run." << endl << endl
        << '\t' << "1)	Forward Selection" << endl
        << '\t' << "2)	Backward Elimination" << endl  
        << '\t' << "3)	Robert's Iterative Deepening" << endl;
        getline (cin, hold);
        stringstream tran(hold);
        tran >> alg;
    }
    

    ifstream mystream;
    mystream.open(type.c_str());
	if (!mystream.is_open()) {
		perror("File not found");
	}
    vector <bool> list;
    vector<vector<shared_ptr<double>>> features;
    features = ReadIn(mystream, features);
	mystream.close();
    cout << "This dataset has " << (features.at(0).size() - 1) << " features ("
    << "not including the class attribute), with " << features.size() << " instances."
    << endl << "Please wait while I normalize the data...    ";
    //print(features);
    Normalize(features);
    cout << "Done!" << endl;
    if(alg == 1) {
    	double t = 100.00;
    	list = Forward_Selection(features, t, features.at(0).size());
    }
    if(alg == 2) list = Backward_Elimination(features);
    if(alg == 3) list = Iterative_Deepening(features, 90.0);
	cout << "solution was found" << endl;
    return 0;
}