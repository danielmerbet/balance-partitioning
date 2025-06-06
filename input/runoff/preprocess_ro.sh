
ncrename -d valid_time,time -v valid_time,time runoff_monthly.nc runoff_monthly_ok.nc

ncks --mk_rec_dmn time runoff_monthly_ok.nc -O runoff_monthly_ok_rec.nc

for ((i=0; i<70; i++)); do
  start=$((i * 12))
  end=$((start + 11))
  echo "Summing months $start to $end for year $((i+1))..."

  ncra -d time,$start,$end runoff_monthly_ok_rec.nc ro_annual_$((i+1)).nc
done

ncrcat ro_annual_*.nc ro_annual.nc

rm ro_annual_*.nc
