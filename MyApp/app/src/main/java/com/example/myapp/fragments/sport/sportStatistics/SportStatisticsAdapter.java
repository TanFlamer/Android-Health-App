package com.example.myapp.fragments.sport.sportStatistics;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SportStatisticsAdapter extends RecyclerView.Adapter<SportStatisticsAdapter.SportRecyclerItemViewHolder> {

    private final Context context;
    private final List<Type> typeList;
    private final HashMap<Type, double[]> sportResults;
    private final HashMap<Type, Boolean> visibilityMap;
    private final SportStatisticsViewModel sportStatisticsViewModel;

    //constructor for sport statistics adapter
    public SportStatisticsAdapter(Context context, HashMap<Type, double[]> sportResults, SportStatisticsViewModel sportStatisticsViewModel){
        this.context = context;
        this.typeList = new ArrayList<>(sportResults.keySet());
        this.sportResults = sportResults;
        visibilityMap = new HashMap<>();
        for(Type type : typeList) visibilityMap.put(type, false);
        this.sportStatisticsViewModel = sportStatisticsViewModel;
    }

    @NonNull
    @Override //get view for each sport statistics
    public SportRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sport_recycler_list_item, parent, false);
        return new SportRecyclerItemViewHolder(view);
    }

    @SuppressLint("SetTextI18n")
    @Override //bind sport statistics to view holder
    public void onBindViewHolder(@NonNull SportRecyclerItemViewHolder holder, int position) {
        Type type = typeList.get(position);
        double[] results = sportResults.get(type);
        holder.titleView.setText(type.getTypeName());
        holder.totalTimeView.setText(String.valueOf(results[0]));
        holder.totalCalorieView.setText(String.valueOf(results[1]));
        holder.totalDaysView.setText(String.valueOf(results[2]));
        holder.averageTimeView.setText(String.valueOf(results[3]));
        holder.averageCalorieView.setText(String.valueOf((results[4])));
        holder.longestTimeView.setText(String.valueOf(results[5]));
        holder.shortestTimeView.setText(String.valueOf(results[6]));
        holder.mostCalorieView.setText(String.valueOf(results[7]));
        holder.leastCalorieView.setText(String.valueOf(results[8]));
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(type)) ? View.VISIBLE : View.GONE);
    }

    @Override //get sport statistics count
    public int getItemCount() {
        return typeList.size();
    }

    //update sport statistics list when sport data changes
    public void updateSportList(HashMap<Type, double[]> newSportResults, String data, String order){
        //clear old sport type list
        typeList.clear();
        //add new sport type list
        typeList.addAll(newSportResults.keySet());
        //clear old sport statistics list
        sportResults.clear();
        //add new sport statistics list
        sportResults.putAll(newSportResults);
        //clear old hidden layout map
        visibilityMap.clear();
        //sort new sport statistics list
        sortSportList(data, order);
    }

    //sort new sport statistics list
    @SuppressLint("NotifyDataSetChanged")
    public void sortSportList(String data, String order){
        //sort sport statistics list
        sportStatisticsViewModel.sortSportStatistics(typeList, sportResults, data, order);
        //hide all hidden layouts
        for(Type type : typeList) visibilityMap.put(type, false);
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    public class SportRecyclerItemViewHolder extends RecyclerView.ViewHolder{

        TextView titleView, totalTimeView, totalCalorieView, averageTimeView, totalDaysView, averageCalorieView, longestTimeView, shortestTimeView, mostCalorieView, leastCalorieView;
        LinearLayout layoutVisible, layoutHidden;

        public SportRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);
            //initialise all components
            initialiseAll();
        }

        //initialise all components
        public void initialiseAll(){
            //find all components by ID
            initialiseViewByID();
            //initialise on click listener
            initialiseOnClickListener();
        }

        //find all components by ID
        public void initialiseViewByID(){
            //find text views by ID
            initialiseTextViews();
            //find layouts by ID
            initialiseLayouts();
        }

        //find text views by ID
        public void initialiseTextViews(){
            titleView = itemView.findViewById(R.id.sportTitle);
            totalTimeView = itemView.findViewById(R.id.sportTotalTime);
            totalCalorieView = itemView.findViewById(R.id.sportTotalCalorie);
            totalDaysView = itemView.findViewById(R.id.sportTotalDays);
            averageTimeView = itemView.findViewById(R.id.sportAverageTime);
            averageCalorieView = itemView.findViewById(R.id.sportAverageCalorie);
            longestTimeView = itemView.findViewById(R.id.sportLongestTime);
            shortestTimeView = itemView.findViewById(R.id.sportShortestTime);
            mostCalorieView = itemView.findViewById(R.id.sportMostCalorie);
            leastCalorieView = itemView.findViewById(R.id.sportLeastCalorie);
        }

        //find layouts by ID
        public void initialiseLayouts(){
            layoutVisible = itemView.findViewById(R.id.sportLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sportLayoutHidden);
        }

        //initialise on click listener
        public void initialiseOnClickListener(){
            layoutVisible.setOnClickListener(view -> {
                //get sport type at position
                Type type = typeList.get(getAdapterPosition());
                //invert hidden layout visibility on click
                visibilityMap.put(type, Boolean.FALSE.equals(visibilityMap.get(type)));
                //notify adapter dataset changed
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
