#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <sys/stat.h>
using namespace Rcpp;
using namespace std;

// function for get file size
Function file_size("file.size");

//' Read data from HIS file for time series and column given by mask
//'
//' @param path Path to HIS file
//' @param mask Indexes of columns to get data. Counting from 1.
//' The time column will be always included.
//' @param startTS Index of starting time step to read. Default read all
//' @param nTS Number of time steps to read. Default read all after and included the StartTS
//' @export
// [[Rcpp::export]]
NumericMatrix readHis(const std::string path, const NumericVector mask,
                      int startTS = 1, int nTS = -1, const long dt = 60) {

  uint64_t fSize;
  double totalTS;
  long nParam, nID, lineSize, thisTS, dataPos;
  std::vector<float> dataLine;
  std::ifstream hFile;


  fSize = as<uint64_t>(file_size(path));
  hFile.open(path, std::ifstream::ate | std::ifstream::binary);
  // Bypass the title at the beginning of his file.
  hFile.seekg(160, ios::beg);
  hFile.read((char*) &nParam, 4);
  hFile.read((char*) &nID, 4);
  dataPos = (168 + 20 * nParam + 24 * nID);
  lineSize = nParam * nID; // except the integer for time
  totalTS = (fSize - dataPos) / 4 / lineSize;
  if ((nTS <= 0) | (nTS > totalTS))
    nTS = totalTS;
  if (startTS < 1)
    startTS = 1;
  if (startTS + nTS > totalTS + 1) {
    Rcout << "There are only " << totalTS << " time steps!" << endl;
    Rcout << "The end record exceeds " << totalTS << ". Check input please!" << endl;
    NumericMatrix ans(0,0);
    hFile.close();
    return ans;
  }
  dataLine.resize(lineSize);
  hFile.clear();
  // recalculate dataPos for the startTS. Minus 1 because C++ count from 0
  dataPos = dataPos + (startTS - 1) * (4 + 4 * lineSize);
  hFile.seekg(dataPos, ios::beg);
  NumericMatrix dataMatrix(nTS, mask.size() + 1);
  NumericVector maskSeq(mask.size());
  for (int i = 0; i < mask.size(); i++) {
    maskSeq[i] = i;
  }
  NumericVector seqNA = maskSeq[is_na(mask)];
  NumericVector seqGET = maskSeq[!is_na(mask)];
  int nSeqNA = seqNA.size();
  int nSeqGET = seqGET.size();
  for (int i = 0; i < nTS; i++) {
    hFile.read((char*)(&thisTS), 4);
    dataMatrix(i, 0) = thisTS * dt;
    // we read the whole data line into memory
    hFile.read((char*)&dataLine[0], 4*lineSize);
    // then take only values that we need, given by the mask
    for (int j=0; j < nSeqGET; j++) {
      dataMatrix(i, seqGET[j] + 1) = dataLine[mask[seqGET[j]]];
    }
    for (int k=0; k < nSeqNA; k++) {
      dataMatrix(i, seqNA[k] + 1) = NA_REAL;
    }
  }
  hFile.close();
  return dataMatrix;
}
