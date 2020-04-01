OUTPUT="$(conda build .)"
echo "${OUTPUT}" > build.txt

storepath="$(grep 'TEST START:' build.txt | sed 's/^.*: //')"
filename="$(echo $storepath | awk -F'/' '{print $6}')"

# grep 'potato:' file.txt | sed 's/^.*: //'
# grep looks for any line that contains the string potato:, then, for each of these lines, # sed replaces (s/// - substitute) any character (.*) from the beginning of the line #(^)until the last occurrence of the sequence : (colon followed by space) with the empty #string (s/...// - substitute the first part with the second part, which is empty)

conda convert --platform all $storepath -o /opt/anaconda3/conda-bld

for VARIABLE in linux-64 linux-32
do
	anaconda upload -i /opt/anaconda3/conda-bld/$VARIABLE/$filename
done

rm build.txt
