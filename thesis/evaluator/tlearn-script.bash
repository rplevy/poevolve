PWD=$(pwd)
NNFODIR="/root/creativity/code/nnfo"
cd $NNFODIR
tlearn -f evaluator -l evaluator.wts -X -s 1752 -V > evaluator.output
cat header evaluator.output footer > evaluator.ratings
cd $PWD
